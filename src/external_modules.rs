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
        Ok(CommandArg {
            name: self.name,
            required: self.required,
            allowed: self.allowed,
            expand_searchpath: self.expand_searchpath,
            when_true,
            default,
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

    /// Validate the module and run its `init` command, if any (mirrors `validateModule`).
    pub fn validate(&self, ngless_version: (i64, i64), temp_dir: &Path) -> NgResult<()> {
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
    for base in &bases {
        let dir = base.join(&modpath);
        let yaml = dir.join("module.yaml");
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
    Err(NgError::new(
        NgErrorType::SystemError,
        format!("Could not find external module '{name}' version {version}."),
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
