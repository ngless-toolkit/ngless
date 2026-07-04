//! External YAML modules (`Modules/<name>.ngm/<version>/module.yaml`), mirroring
//! `NGLess/ExternalModules.hs`. An external module defines a set of NGLess functions that shell
//! out to executables, with their arguments encoded onto the command line.
//!
//! This module handles the data model, YAML loading (`find_load`), the function signatures
//! exposed to the type checker (`functions_for_typecheck`), and module validation (`validate`,
//! which runs the optional `init` command). The actual command execution lives in
//! `interpret.rs` (`execute_external_command`), since it needs interpreter state (temp dir,
//! search path, FASTQ concatenation).

use std::path::{Path, PathBuf};
use std::process::Command as ProcCommand;

use serde::Deserialize;

use crate::ast::{FuncName, NGLType};
use crate::errors::{NgError, NgErrorType, NgResult};
use crate::modules::{ArgCheck, ArgInformation, Function};

/// The kind of a command argument, mirroring the `atype` field.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArgKind {
    Flag,
    Option,
    Int,
    Str,
    ReadSet,
    Counts,
    MappedReadSet,
    SequenceSet,
}

impl ArgKind {
    fn parse(s: &str) -> NgResult<ArgKind> {
        Ok(match s {
            "flag" => ArgKind::Flag,
            "option" => ArgKind::Option,
            "int" => ArgKind::Int,
            "str" => ArgKind::Str,
            "readset" => ArgKind::ReadSet,
            "counts" => ArgKind::Counts,
            "mappedreadset" => ArgKind::MappedReadSet,
            "sequenceset" => ArgKind::SequenceSet,
            other => {
                return Err(NgError::new(
                    NgErrorType::SystemError,
                    format!("unknown argument type {other}"),
                ))
            }
        })
    }

    fn ngltype(&self) -> NGLType {
        match self {
            ArgKind::Flag => NGLType::Bool,
            ArgKind::Option => NGLType::Symbol,
            ArgKind::Int => NGLType::Integer,
            ArgKind::Str => NGLType::String,
            ArgKind::ReadSet => NGLType::ReadSet,
            ArgKind::Counts => NGLType::Counts,
            ArgKind::MappedReadSet => NGLType::MappedReadSet,
            ArgKind::SequenceSet => NGLType::SequenceSet,
        }
    }
}

/// The base file type of a file-backed argument (mirrors `FileTypeBase`). Determines how the
/// interpreter materialises a `readset`/`mappedreadset`/`counts` value into a file for the module.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FileTypeBase {
    FastqSingle,  // "fq1"
    FastqPair,    // "fq2"
    FastqTriplet, // "fq3"
    Sam,          // "sam"
    Bam,          // "bam"
    SamOrBam,     // "sam_or_bam"
    Tsv,          // "tsv"
}

impl FileTypeBase {
    fn parse(s: &str) -> NgResult<FileTypeBase> {
        Ok(match s {
            "fq1" => FileTypeBase::FastqSingle,
            "fq2" => FileTypeBase::FastqPair,
            "fq3" => FileTypeBase::FastqTriplet,
            "sam" => FileTypeBase::Sam,
            "bam" => FileTypeBase::Bam,
            "sam_or_bam" => FileTypeBase::SamOrBam,
            "tsv" => FileTypeBase::Tsv,
            other => {
                return Err(NgError::new(
                    NgErrorType::SystemError,
                    format!("unknown file type '{other}'"),
                ))
            }
        })
    }
}

/// The `filetype`/`can_gzip`/`can_bzip2`/`can_stream` payload attached to a file-backed argument
/// (mirrors `FileType`). `can_gzip`/`can_bzip2` control whether a compressed input file is passed
/// through as-is or decompressed before being handed to the module command.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FileType {
    pub base: FileTypeBase,
    pub can_gzip: bool,
    pub can_bzip2: bool,
    pub can_stream: bool,
}

/// A default value for a command argument (mirrors `cargDef`).
#[derive(Clone, Debug)]
pub enum DefaultVal {
    Bool(bool),
    Sym(String),
    Int(i64),
    Str(String),
}

/// One command argument (mirrors `CommandArgument`/`ArgInformation`).
#[derive(Clone, Debug)]
pub struct CommandArg {
    pub name: String, // empty for `arg1`
    pub required: bool,
    pub kind: ArgKind,
    /// Allowed values for an `option` argument.
    pub allowed: Vec<String>,
    /// The default value, used when the argument is not supplied (mirrors `cargDef`).
    pub default: Option<DefaultVal>,
    /// For a `flag`/`option`, the command-line tokens emitted when it is true (`when-true`).
    pub when_true: Option<Vec<String>>,
    /// For a `str` argument, whether to expand `<...>` search-path placeholders.
    pub expand_searchpath: bool,
    /// For a `readset`/`counts`/`mappedreadset` argument, the declared file-type payload (mirrors
    /// the `FileInfo` command extra). `None` when the argument has no `filetype` field.
    pub file_info: Option<FileType>,
}

/// The return specification of a command (mirrors `CommandReturn`).
#[derive(Clone, Debug)]
pub struct CommandReturn {
    pub rtype: NGLType,
    pub name: String,
    pub extension: String,
}

/// A single NGLess function provided by an external module (mirrors `Command`).
#[derive(Clone, Debug)]
pub struct Command {
    pub ngl_name: String,
    pub arg0: String,
    pub arg1: CommandArg,
    pub additional: Vec<CommandArg>,
    pub ret: CommandReturn,
}

/// A loaded external module (mirrors `ExternalModule`).
#[derive(Clone, Debug)]
pub struct ExternalModule {
    pub name: String,
    pub version: String,
    pub dir: PathBuf,
    pub functions: Vec<Command>,
    pub min_ngless_version: Option<(i64, i64)>,
    pub min_version_reason: String,
    pub init_cmd: Option<String>,
    pub init_args: Vec<String>,
    pub citations: Vec<String>,
}

// --- raw YAML structures --------------------------------------------------

#[derive(Deserialize)]
struct RawModule {
    name: String,
    version: String,
    #[serde(rename = "min-ngless-version")]
    min_ngless_version: Option<RawMinVersion>,
    init: Option<RawInit>,
    #[serde(default)]
    functions: Vec<RawCommand>,
    citation: Option<String>,
    #[serde(default)]
    citations: Vec<String>,
}

#[derive(Deserialize)]
struct RawMinVersion {
    #[serde(rename = "min-version")]
    min_version: String,
    reason: String,
}

#[derive(Deserialize)]
struct RawInit {
    init_cmd: String,
    #[serde(default)]
    init_args: Vec<String>,
}

#[derive(Deserialize)]
struct RawCommand {
    #[serde(rename = "nglName")]
    ngl_name: String,
    arg0: String,
    arg1: RawArg,
    #[serde(default)]
    additional: Vec<RawArg>,
    #[serde(rename = "return")]
    ret: Option<RawReturn>,
}

#[derive(Deserialize)]
struct RawArg {
    #[serde(default)]
    name: String,
    #[serde(default)]
    required: bool,
    atype: String,
    #[serde(default)]
    allowed: Vec<String>,
    #[serde(default)]
    def: Option<serde_yaml::Value>,
    #[serde(rename = "when-true", default)]
    when_true: Option<serde_yaml::Value>,
    #[serde(rename = "expand_searchpath", default)]
    expand_searchpath: bool,
    #[serde(default)]
    filetype: Option<String>,
    #[serde(rename = "can_gzip", default)]
    can_gzip: bool,
    #[serde(rename = "can_bzip2", default)]
    can_bzip2: bool,
    #[serde(rename = "can_stream", default)]
    can_stream: bool,
}

#[derive(Deserialize)]
struct RawReturn {
    rtype: String,
    #[serde(default)]
    name: String,
    #[serde(default)]
    extension: String,
}

impl RawArg {
    fn convert(self) -> NgResult<CommandArg> {
        let kind = ArgKind::parse(&self.atype)?;
        let when_true = self.when_true.map(value_to_strings);
        let default = self.def.and_then(|v| default_for(&kind, v));
        // The `FileInfo` payload only applies to file-backed arguments and only when a `filetype`
        // is declared (mirrors `Aeson.parseJSON <|> return Nothing`, where the parse fails and
        // falls back to `Nothing` if the `filetype` key is absent).
        let file_info = match kind {
            ArgKind::ReadSet | ArgKind::Counts | ArgKind::MappedReadSet => match &self.filetype {
                Some(ft) => Some(FileType {
                    base: FileTypeBase::parse(ft)?,
                    can_gzip: self.can_gzip,
                    can_bzip2: self.can_bzip2,
                    can_stream: self.can_stream,
                }),
                None => None,
            },
            _ => None,
        };
        Ok(CommandArg {
            name: self.name,
            required: self.required,
            allowed: self.allowed,
            expand_searchpath: self.expand_searchpath,
            when_true,
            default,
            file_info,
            kind,
        })
    }
}

/// Interpret a YAML `def` value according to the argument kind.
fn default_for(kind: &ArgKind, v: serde_yaml::Value) -> Option<DefaultVal> {
    match kind {
        ArgKind::Flag => v.as_bool().map(DefaultVal::Bool),
        ArgKind::Option => v.as_str().map(|s| DefaultVal::Sym(s.to_string())),
        ArgKind::Int => v.as_i64().map(DefaultVal::Int),
        ArgKind::Str => v.as_str().map(|s| DefaultVal::Str(s.to_string())),
        _ => None,
    }
}

/// A `when-true` value may be a single string or a list of strings.
fn value_to_strings(v: serde_yaml::Value) -> Vec<String> {
    match v {
        serde_yaml::Value::String(s) => vec![s],
        serde_yaml::Value::Sequence(seq) => seq
            .into_iter()
            .filter_map(|e| e.as_str().map(|s| s.to_string()))
            .collect(),
        _ => Vec::new(),
    }
}

fn ret_ngltype(s: &str) -> NgResult<NGLType> {
    Ok(match s {
        "void" => NGLType::Void,
        "counts" => NGLType::Counts,
        "readset" => NGLType::ReadSet,
        "mappedreadset" => NGLType::MappedReadSet,
        "sequenceset" => NGLType::SequenceSet,
        other => {
            return Err(NgError::new(
                NgErrorType::SystemError,
                format!("Cannot parse unknown type '{other}'"),
            ))
        }
    })
}

fn parse_min_version(s: &str) -> Option<(i64, i64)> {
    let mut it = s.split('.');
    let maj = it.next()?.parse().ok()?;
    let min = it.next().unwrap_or("0").parse().ok()?;
    Some((maj, min))
}

impl ExternalModule {
    fn from_raw(raw: RawModule, dir: PathBuf) -> NgResult<ExternalModule> {
        let mut functions = Vec::new();
        for rc in raw.functions {
            let ret = match rc.ret {
                None => CommandReturn {
                    rtype: NGLType::Void,
                    name: String::new(),
                    extension: String::new(),
                },
                Some(r) => {
                    let rtype = ret_ngltype(&r.rtype)?;
                    CommandReturn {
                        rtype,
                        name: r.name,
                        extension: r.extension,
                    }
                }
            };
            let additional = rc
                .additional
                .into_iter()
                .map(RawArg::convert)
                .collect::<NgResult<Vec<_>>>()?;
            functions.push(Command {
                ngl_name: rc.ngl_name,
                arg0: rc.arg0,
                arg1: rc.arg1.convert()?,
                additional,
                ret,
            });
        }
        let (min_ngless_version, min_version_reason) = match raw.min_ngless_version {
            Some(mv) => (parse_min_version(&mv.min_version), mv.reason),
            None => (None, String::new()),
        };
        let (init_cmd, init_args) = match raw.init {
            Some(i) => (Some(i.init_cmd), i.init_args),
            None => (None, Vec::new()),
        };
        let mut citations = raw.citation.map(|c| vec![c]).unwrap_or_default();
        citations.extend(raw.citations);
        Ok(ExternalModule {
            name: raw.name,
            version: raw.version,
            dir,
            functions,
            min_ngless_version,
            min_version_reason,
            init_cmd,
            init_args,
            citations,
        })
    }

    /// The function signatures exposed to the type checker (mirrors `asFunction`).
    pub fn functions_for_typecheck(&self) -> Vec<Function> {
        self.functions
            .iter()
            .map(|c| Function {
                name: FuncName(c.ngl_name.clone()),
                arg_type: Some(c.arg1.kind.ngltype()),
                arg_checks: Vec::new(),
                ret_type: c.ret.rtype.clone(),
                kwargs: c.additional.iter().map(arg_information).collect(),
                allows_auto_comprehension: false,
                checks: Vec::new(),
            })
            .collect()
    }

    pub fn find_command(&self, name: &str) -> Option<&Command> {
        self.functions.iter().find(|c| c.ngl_name == name)
    }

    /// Attempt to find bugs in the module definition (mirrors `checkSyntax`): `arg1` may not carry
    /// a `name`, each `additional` argument must be named, and every file-typed argument must use a
    /// legal `atype`/`filetype` combination.
    fn check_syntax(&self) -> NgResult<()> {
        for f in &self.functions {
            if !f.arg1.name.is_empty() {
                return Err(NgError::script(
                    "Error in module.yaml: `arg1` cannot have a 'name' attribute",
                ));
            }
            check_arg_types(&f.arg1)?;
            for a in &f.additional {
                if a.name.is_empty() {
                    return Err(NgError::script(
                        "Error in module.yaml: `additional` argument is missing a name",
                    ));
                }
                check_arg_types(a)?;
            }
        }
        Ok(())
    }

    /// Validate the module and run its `init` command, if any (mirrors `validateModule`).
    pub fn validate(&self, ngless_version: (i64, i64), temp_dir: &Path) -> NgResult<()> {
        self.check_syntax()?;
        if let Some(minv) = self.min_ngless_version {
            if minv > ngless_version {
                return Err(NgError::script(format!(
                    "Current NGLess version is too old for loading module '{}'.\n\
                     Version {}.{} is required.\nReason: {}",
                    self.name, minv.0, minv.1, self.min_version_reason
                )));
            }
        }
        if let Some(init) = &self.init_cmd {
            let exe = self.dir.join(init);
            let output = ProcCommand::new(&exe)
                .args(&self.init_args)
                .envs(module_env(&self.dir, temp_dir))
                .output()
                .map_err(|e| {
                    NgError::new(
                        NgErrorType::SystemError,
                        format!(
                            "Error loading module {}: could not run {init}: {e}",
                            self.name
                        ),
                    )
                })?;
            if !output.status.success() {
                return Err(NgError::new(
                    NgErrorType::SystemError,
                    format!(
                        "Error loading module {} (init command {init} failed)\n\tstdout='{}'\n\tstderr='{}'",
                        self.name,
                        String::from_utf8_lossy(&output.stdout),
                        String::from_utf8_lossy(&output.stderr)
                    ),
                ));
            }
        }
        Ok(())
    }
}

/// Check that a file-typed argument uses a legal `atype`/`filetype` combination (mirrors
/// `checkArgsTypes` + `legalNGLTypeFileTypeCombos`). Arguments without a `filetype` payload pass.
fn check_arg_types(a: &CommandArg) -> NgResult<()> {
    if let Some(ft) = &a.file_info {
        let legal = matches!(
            (&a.kind, &ft.base),
            (ArgKind::ReadSet, FileTypeBase::FastqSingle)
                | (ArgKind::ReadSet, FileTypeBase::FastqPair)
                | (ArgKind::ReadSet, FileTypeBase::FastqTriplet)
                | (ArgKind::MappedReadSet, FileTypeBase::Sam)
                | (ArgKind::MappedReadSet, FileTypeBase::Bam)
                | (ArgKind::MappedReadSet, FileTypeBase::SamOrBam)
                | (ArgKind::Counts, FileTypeBase::Tsv)
        );
        if !legal {
            return Err(NgError::script(
                "Illegal combination of options for atype/filetype",
            ));
        }
    }
    Ok(())
}

/// Build the `ArgInformation` for the type checker from a command argument.
fn arg_information(a: &CommandArg) -> ArgInformation {
    let checks = if a.kind == ArgKind::Option && !a.allowed.is_empty() {
        vec![ArgCheck::Symbol(a.allowed.clone())]
    } else {
        Vec::new()
    };
    ArgInformation {
        name: a.name.clone(),
        required: a.required,
        atype: a.kind.ngltype(),
        checks,
    }
}

/// Environment variables exposed to module processes (mirrors `nglessEnv`).
pub fn module_env(module_dir: &Path, temp_dir: &Path) -> Vec<(String, String)> {
    let tmp = temp_dir.to_string_lossy().into_owned();
    let mut env = vec![
        (
            "NGLESS_MODULE_DIR".to_string(),
            module_dir.to_string_lossy().into_owned(),
        ),
        ("NGLESS_NR_CORES".to_string(), "1".to_string()),
        ("TMPDIR".to_string(), tmp.clone()),
        ("TMP".to_string(), tmp.clone()),
        ("TEMPDIR".to_string(), tmp.clone()),
        ("TEMP".to_string(), tmp),
    ];
    if let Ok(exe) = std::env::current_exe() {
        env.push((
            "NGLESS_NGLESS_BIN".to_string(),
            exe.to_string_lossy().into_owned(),
        ));
    }
    env
}

/// Find and load an external module (mirrors `findLoad`). Searches the current directory, then the
/// global and user data directories, for `Modules/<name>.ngm/<version>/module.yaml`.
pub fn find_load(name: &str, version: &str, data_dirs: &[String]) -> NgResult<ExternalModule> {
    let modpath = Path::new("Modules")
        .join(format!("{name}.ngm"))
        .join(version);
    let mut bases: Vec<PathBuf> = vec![PathBuf::from(".")];
    bases.extend(data_dirs.iter().map(PathBuf::from));
    let mut searched: Vec<PathBuf> = Vec::with_capacity(bases.len());
    for base in &bases {
        let dir = base.join(&modpath);
        let yaml = dir.join("module.yaml");
        crate::output::trace(
            0,
            &format!(
                "Looking for module '{name}' version {version} at {}",
                yaml.display()
            ),
        );
        searched.push(yaml.clone());
        if yaml.is_file() {
            let text = std::fs::read_to_string(&yaml).map_err(|e| {
                NgError::new(
                    NgErrorType::SystemError,
                    format!("Could not read module file {}: {e}", yaml.display()),
                )
            })?;
            let raw: RawModule = serde_yaml::from_str(&text).map_err(|e| {
                NgError::new(
                    NgErrorType::SystemError,
                    format!(
                        "Could not load module file {}. Error was `{e}`",
                        yaml.display()
                    ),
                )
            })?;
            let module = ExternalModule::from_raw(raw, dir)?;
            check_compatible(name, version, &module)?;
            return Ok(module);
        }
    }
    let locations = searched
        .iter()
        .map(|p| format!("\t{}", p.display()))
        .collect::<Vec<_>>()
        .join("\n");
    // The requested version was not found; check whether other versions of the same module
    // exist so we can point the user at them.
    let mut available: Vec<String> = Vec::new();
    let namedir = Path::new("Modules").join(format!("{name}.ngm"));
    for base in &bases {
        let dir = base.join(&namedir);
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            if entry.path().join("module.yaml").is_file() {
                if let Some(v) = entry.file_name().to_str() {
                    if !available.iter().any(|a| a == v) {
                        available.push(v.to_string());
                    }
                }
            }
        }
    }
    let available_msg = if available.is_empty() {
        String::new()
    } else {
        available.sort();
        format!(
            "\nOther versions of module '{name}' are available: {}.",
            available.join(", ")
        )
    };
    Err(NgError::new(
        NgErrorType::SystemError,
        format!(
            "Could not find external module '{name}' version {version}.\n\
             The following locations were searched:\n{locations}{available_msg}"
        ),
    ))
}

/// Check that the requested version matches the module file (mirrors `checkCompatible`: only the
/// major and minor components are compared).
fn check_compatible(name: &str, version: &str, module: &ExternalModule) -> NgResult<()> {
    let norm = |v: &str| -> Option<(String, String)> {
        let mut it = v.split('.');
        Some((it.next()?.to_string(), it.next()?.to_string()))
    };
    if norm(version) != norm(&module.version) {
        return Err(NgError::new(
            NgErrorType::SystemError,
            format!(
                "Mismatched version information when loading module `{name}`.\n\t\
                 Expected {version} but file contains '{}'.",
                module.version
            ),
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn load(yaml: &str) -> NgResult<ExternalModule> {
        let raw: RawModule = serde_yaml::from_str(yaml).expect("valid YAML");
        ExternalModule::from_raw(raw, PathBuf::from("."))
    }

    #[test]
    fn parses_file_info_payload() {
        let m = load(
            "name: t\nversion: '0.0'\nfunctions:\n  - nglName: f\n    arg0: ./x.sh\n    \
             arg1: {atype: readset, filetype: fq1, can_gzip: true}\n",
        )
        .unwrap();
        let ft = m.functions[0].arg1.file_info.as_ref().unwrap();
        assert_eq!(ft.base, FileTypeBase::FastqSingle);
        assert!(ft.can_gzip);
        assert!(!ft.can_bzip2);
    }

    #[test]
    fn no_file_info_without_filetype() {
        let m = load(
            "name: t\nversion: '0.0'\nfunctions:\n  - nglName: f\n    arg0: ./x.sh\n    \
             arg1: {atype: readset}\n",
        )
        .unwrap();
        assert!(m.functions[0].arg1.file_info.is_none());
    }

    #[test]
    fn sequenceset_return_type() {
        let m = load(
            "name: t\nversion: '0.0'\nfunctions:\n  - nglName: f\n    arg0: ./x.sh\n    \
             arg1: {atype: readset}\n    return: {rtype: sequenceset, name: contigs, extension: fna}\n",
        )
        .unwrap();
        assert_eq!(m.functions[0].ret.rtype, NGLType::SequenceSet);
    }

    #[test]
    fn check_syntax_rejects_illegal_filetype_combo() {
        // `counts` may only pair with `tsv`, not `sam`.
        let m = load(
            "name: t\nversion: '0.0'\nfunctions:\n  - nglName: f\n    arg0: ./x.sh\n    \
             arg1: {atype: counts, filetype: sam}\n",
        )
        .unwrap();
        assert!(m.check_syntax().is_err());
    }

    #[test]
    fn check_syntax_rejects_named_arg1() {
        let m = load(
            "name: t\nversion: '0.0'\nfunctions:\n  - nglName: f\n    arg0: ./x.sh\n    \
             arg1: {name: input, atype: readset}\n",
        )
        .unwrap();
        assert!(m.check_syntax().is_err());
    }

    #[test]
    fn check_syntax_rejects_unnamed_additional() {
        let m = load(
            "name: t\nversion: '0.0'\nfunctions:\n  - nglName: f\n    arg0: ./x.sh\n    \
             arg1: {atype: readset}\n    additional:\n      - {atype: flag}\n",
        )
        .unwrap();
        assert!(m.check_syntax().is_err());
    }

    #[test]
    fn check_syntax_accepts_legal_module() {
        let m = load(
            "name: t\nversion: '0.0'\nfunctions:\n  - nglName: f\n    arg0: ./x.sh\n    \
             arg1: {atype: mappedreadset, filetype: bam}\n    additional:\n      \
             - {name: verbose, atype: flag}\n",
        )
        .unwrap();
        assert!(m.check_syntax().is_ok());
    }
}
