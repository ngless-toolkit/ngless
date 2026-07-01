//! Module / function metadata, mirroring `NGLess/Modules.hs` and the builtin tables in
//! `NGLess/BuiltinFunctions.hs`.
//!
//! Only what the front end needs is ported: the builtin function and method signatures used
//! by type checking and validation. Module loading, references and `runFunction` are deferred
//! to later milestones.

use crate::ast::{FuncName, MethodName, NGLType};

/// The current canonical version for the built-in standard modules (`samtools`/`parallel`/
/// `mocat`/...). Going forward, internal modules track the ngless language version, so scripts
/// should `import "<module>" version "1.6"`. Older versions still load (with the latest behaviour)
/// but the import loop emits a deprecation warning (see `cli::run_script`).
pub const CURRENT_MODULE_VERSION: &str = "1.6";

/// A `major.minor` NGLess language version. Ordering is lexicographic (major, then minor).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NGLVersion {
    pub major: i64,
    pub minor: i64,
}

impl NGLVersion {
    pub fn new(major: i64, minor: i64) -> Self {
        NGLVersion { major, minor }
    }
}

/// Checks for a single argument.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArgCheck {
    /// For symbol arguments, the list of allowed values.
    Symbol(Vec<String>),
    FileReadable,
    FileWritable,
    /// First version where this argument may be used.
    MinVersion(i64, i64),
    /// Deprecated since the given version, with a reason.
    Deprecated(i64, i64, String),
}

/// Checks for a function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionCheck {
    /// First version where this function may be used.
    MinNGLessVersion(i64, i64),
    /// The function is pure: its return value must be assigned.
    ReturnAssigned,
    /// Version when behaviour changed, with a reason.
    NGLVersionIncompatibleChange(i64, i64, String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArgInformation {
    pub name: String,
    pub required: bool,
    pub atype: NGLType,
    pub checks: Vec<ArgCheck>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub name: FuncName,
    /// If it takes an unnamed argument, its type.
    pub arg_type: Option<NGLType>,
    pub arg_checks: Vec<ArgCheck>,
    pub ret_type: NGLType,
    pub kwargs: Vec<ArgInformation>,
    /// If true, calling with `[arg_type]` returns `[ret_type]`.
    pub allows_auto_comprehension: bool,
    pub checks: Vec<FunctionCheck>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MethodInfo {
    pub name: MethodName,
    pub self_type: NGLType,
    /// The unnamed argument is called "__0".
    pub arg_type: Option<NGLType>,
    pub return_type: NGLType,
    pub kwargs: Vec<ArgInformation>,
    pub is_pure: bool,
    pub checks: Vec<FunctionCheck>,
}

// --- builders -------------------------------------------------------------

pub use builders::{builtin_functions, builtin_methods, module_constants, module_functions};

/// The builtin tables live in an inner module so that `use NGLType::*` (which makes `String`,
/// `Integer`, ... name `NGLType` variants) does not clash with the `std` `String` used in the
/// struct field types above.
mod builders {
    use super::{ArgCheck, ArgInformation, Function, FunctionCheck, MethodInfo, NGLVersion};
    use crate::ast::{FuncName, MethodName, NGLType};
    use NGLType::*;

    fn list(t: NGLType) -> NGLType {
        NGLType::List(Box::new(t))
    }

    fn arg(name: &str, required: bool, atype: NGLType, checks: Vec<ArgCheck>) -> ArgInformation {
        ArgInformation {
            name: name.into(),
            required,
            atype,
            checks,
        }
    }

    fn sym(values: &[&str]) -> ArgCheck {
        ArgCheck::Symbol(values.iter().map(|s| s.to_string()).collect())
    }

    fn func(
        name: &str,
        arg_type: Option<NGLType>,
        arg_checks: Vec<ArgCheck>,
        ret_type: NGLType,
        kwargs: Vec<ArgInformation>,
        checks: Vec<FunctionCheck>,
    ) -> Function {
        Function {
            name: FuncName(name.into()),
            arg_type,
            arg_checks,
            ret_type,
            kwargs,
            allows_auto_comprehension: false,
            checks,
        }
    }

    fn print_type() -> NGLType {
        Union(vec![String, Integer, Double])
    }

    /// The builtin functions, ported from `builtinFunctions` in BuiltinFunctions.hs. The `write`
    /// return type depends on the language version.
    pub fn builtin_functions(ver: NGLVersion) -> Vec<Function> {
        use ArgCheck::*;
        use FunctionCheck::*;

        let write_ret = if ver >= NGLVersion::new(1, 4) {
            String
        } else {
            Void
        };

        vec![
            func(
                "fastq",
                Some(String),
                vec![FileReadable],
                ReadSet,
                fastq_args(),
                vec![],
            ),
            func(
                "paired",
                Some(String),
                vec![FileReadable],
                ReadSet,
                paired_args(),
                vec![],
            ),
            func(
                "group",
                Some(list(ReadSet)),
                vec![],
                ReadSet,
                group_args(),
                vec![],
            ),
            func(
                "samfile",
                Some(String),
                vec![FileReadable],
                MappedReadSet,
                samfile_args(),
                vec![],
            ),
            func(
                "unique",
                Some(ReadSet),
                vec![],
                ReadSet,
                unique_args(),
                vec![ReturnAssigned],
            ),
            func(
                "preprocess",
                Some(ReadSet),
                vec![],
                ReadSet,
                preprocess_args(),
                vec![ReturnAssigned],
            ),
            func(
                "substrim",
                Some(Read),
                vec![],
                Read,
                substrim_args(),
                vec![ReturnAssigned],
            ),
            func(
                "endstrim",
                Some(Read),
                vec![],
                Read,
                endstrim_args(),
                vec![ReturnAssigned],
            ),
            func(
                "smoothtrim",
                Some(Read),
                vec![],
                Read,
                smoothtrim_args(),
                vec![ReturnAssigned, MinNGLessVersion(0, 11)],
            ),
            func(
                "map",
                Some(ReadSet),
                vec![],
                MappedReadSet,
                map_args(),
                vec![ReturnAssigned],
            ),
            func(
                "mapstats",
                Some(MappedReadSet),
                vec![],
                Counts,
                vec![],
                vec![],
            ),
            func(
                "select",
                Some(MappedReadSet),
                vec![],
                MappedReadSet,
                select_args(),
                vec![],
            ),
            func(
                "count",
                Some(MappedReadSet),
                vec![],
                Counts,
                count_args(),
                vec![ReturnAssigned],
            ),
            func(
                "__check_count",
                Some(MappedReadSet),
                vec![],
                Counts,
                count_check_args(),
                vec![],
            ),
            func(
                "countfile",
                Some(String),
                vec![FileReadable],
                Counts,
                vec![],
                vec![ReturnAssigned],
            ),
            // From the always-loaded builtin "as_reads" module (BuiltinModules/AsReads.hs).
            {
                let mut f = func(
                    "as_reads",
                    Some(MappedReadSet),
                    vec![],
                    ReadSet,
                    vec![],
                    vec![ReturnAssigned],
                );
                f.allows_auto_comprehension = true;
                f
            },
            // Also from the "as_reads" module: drop the singleton reads from a read set.
            {
                let mut f = func(
                    "discard_singles",
                    Some(ReadSet),
                    vec![],
                    ReadSet,
                    vec![],
                    vec![MinNGLessVersion(1, 1), ReturnAssigned],
                );
                f.allows_auto_comprehension = true;
                f
            },
            // From the always-loaded builtin "stats" module (BuiltinModules/QCStats.hs).
            func(
                "qcstats",
                Some(NGLType::Symbol),
                vec![sym(&["fastq", "mapping"])],
                Counts,
                vec![],
                vec![
                    ReturnAssigned,
                    NGLVersionIncompatibleChange(0, 8, "".to_string()),
                ],
            ),
            func("write", Some(Any), vec![], write_ret, write_args(), vec![]),
            func("print", Some(print_type()), vec![], Void, vec![], vec![]),
            func("println", Some(print_type()), vec![], Void, vec![], vec![]),
            func(
                "read_int",
                Some(String),
                vec![],
                Integer,
                vec![arg("on_empty_return", false, Integer, vec![])],
                vec![],
            ),
            func(
                "read_double",
                Some(String),
                vec![],
                Double,
                vec![arg("on_empty_return", false, Double, vec![])],
                vec![],
            ),
            func("__assert", Some(Bool), vec![], Void, vec![], vec![]),
            func(
                "readlines",
                Some(String),
                vec![FileReadable],
                list(String),
                vec![],
                vec![ReturnAssigned],
            ),
            func(
                "__merge_samfiles",
                Some(list(String)),
                vec![],
                MappedReadSet,
                vec![],
                vec![],
            ),
            // From the always-loaded builtin "assemble" module (BuiltinModules/Assemble.hs):
            // megahit assembly of a read set into a FASTA sequence set.
            func(
                "assemble",
                Some(ReadSet),
                vec![],
                SequenceSet,
                vec![arg("__extra_megahit_args", false, list(String), vec![])],
                vec![
                    NGLVersionIncompatibleChange(
                        1,
                        4,
                        "Megahit version was updated which significantly changes results"
                            .to_string(),
                    ),
                    ReturnAssigned,
                ],
            ),
            // From the always-loaded builtin "orffind" module (BuiltinModules/ORFFind.hs):
            // prodigal gene prediction. Takes a sequence set (typed as String), returns the
            // predicted-genes FASTA filename.
            func(
                "orf_find",
                Some(String),
                vec![],
                Filename,
                vec![
                    arg("is_metagenome", true, Bool, vec![]),
                    arg("include_fragments", false, Bool, vec![MinVersion(1, 1)]),
                    arg("coords_out", false, String, vec![FileWritable]),
                    arg("prots_out", false, String, vec![FileWritable]),
                ],
                vec![ReturnAssigned],
            ),
            // From the always-loaded builtin "load_directory" module
            // (BuiltinModules/LoadDirectory.hs).
            func(
                "load_fastq_directory",
                Some(String),
                vec![],
                ReadSet,
                load_directory_args(),
                vec![],
            ),
            // From the always-loaded builtin "samples" module (BuiltinModules/Samples.hs).
            {
                let mut f = func(
                    "load_sample_list",
                    Some(String),
                    vec![FileReadable],
                    list(ReadSet),
                    vec![],
                    vec![MinNGLessVersion(1, 5), ReturnAssigned],
                );
                f.allows_auto_comprehension = true;
                f
            },
            {
                let mut f = func(
                    "load_sample_from_yaml",
                    Some(String),
                    vec![FileReadable],
                    ReadSet,
                    vec![arg("sample", true, String, vec![])],
                    vec![MinNGLessVersion(1, 5), ReturnAssigned],
                );
                f.allows_auto_comprehension = true;
                f
            },
        ]
    }

    /// Functions contributed by an imported module (mirrors the `modFunctions` of the standard
    /// modules). Returns `None` for an unknown module/version. Only the `samtools` module is
    /// supported so far (`StandardModules/Samtools.hs`).
    pub fn module_functions(name: &str, version: &str) -> Option<Vec<Function>> {
        match (name, version) {
            ("samtools", "0.0") => Some(vec![samtools_sort_function()]),
            // "1.6" is the current canonical version and maps to the latest behaviour (same as
            // the historical "1.0"/"0.1"); see `CURRENT_MODULE_VERSION`.
            ("samtools", "1.0") | ("samtools", "0.1") | ("samtools", "1.6") => {
                Some(vec![samtools_sort_function(), samtools_view_function()])
            }
            // The `mocat` module (StandardModules/Mocat.hs) exposes `load_mocat_sample`, a thin
            // wrapper over `load_fastq_directory`. The standard module is loaded by name, so any
            // requested version is accepted.
            ("mocat", _) => Some(vec![load_mocat_sample_function()]),
            // The `example` demo module (StandardModules/Example.hs).
            ("example", _) => Some(vec![example_function()]),
            // The `parallel` module (StandardModules/Parallel.hs). `run_for_all`/
            // `run_for_all_samples` (and the relaxed `collect` signature) are only available in
            // version 1.1+; "1.6" (the current canonical version) maps to that latest behaviour.
            ("parallel", v) => Some(parallel_functions(
                v == "1.1" || v == super::CURRENT_MODULE_VERSION,
            )),
            // The `minimap2` module (StandardModules/Minimap2.hs) exposes no functions; importing
            // it only activates the minimap2 mapper (see `active_mappers` in cli.rs).
            ("minimap2", _) => Some(vec![]),
            // The `batch` module (StandardModules/Batch.hs) exposes no functions; it only
            // contributes the `JOBINDEX_*` constants (see `module_constants`) and, at load time,
            // overrides the worker thread count from the batch scheduler's CPU allotment (see the
            // import loop in cli.rs).
            ("batch", _) => Some(vec![]),
            _ => None,
        }
    }

    fn parallel_functions(include_for_all: bool) -> Vec<Function> {
        let mut fs = vec![
            // lock1: a list of strings or read sets -> one chosen entry (a string).
            func(
                "lock1",
                Some(Union(vec![list(String), list(ReadSet)])),
                vec![],
                String,
                vec![],
                vec![],
            ),
            collect_function(include_for_all),
            func(
                "set_parallel_tag",
                Some(String),
                vec![],
                String,
                vec![],
                vec![],
            ),
            // __paste: internal test helper exposing the counts-merging machinery.
            func(
                "__paste",
                Some(list(String)),
                vec![],
                String,
                vec![
                    arg("ofile", true, String, vec![ArgCheck::FileWritable]),
                    arg("headers", true, list(String), vec![]),
                    arg("matching_rows", false, Bool, vec![]),
                ],
                vec![],
            ),
        ];
        if include_for_all {
            fs.push(func(
                "run_for_all",
                Some(list(String)),
                vec![],
                String,
                vec![arg("tag", false, String, vec![])],
                vec![],
            ));
            fs.push(func(
                "run_for_all_samples",
                Some(list(ReadSet)),
                vec![],
                ReadSet,
                vec![arg("tag", false, String, vec![])],
                vec![],
            ));
        }
        fs
    }

    /// `collect()` (mirrors `collectFunction`). `current`/`allneeded` are required except in
    /// version 1.1+, where the `run_for_all` transform injects them.
    fn collect_function(is_v11: bool) -> Function {
        func(
            "collect",
            Some(Counts),
            vec![],
            Void,
            vec![
                arg("current", !is_v11, String, vec![]),
                arg("allneeded", !is_v11, list(String), vec![]),
                arg("ofile", true, String, vec![ArgCheck::FileWritable]),
                arg("__can_move", false, Bool, vec![]),
                arg("comment", false, String, vec![]),
                arg(
                    "auto_comments",
                    false,
                    list(Symbol),
                    vec![sym(&["date", "script", "hash"])],
                ),
            ],
            vec![],
        )
    }

    /// Constants contributed by an imported standard module (mirrors `modConstants`). The
    /// `example` and `batch` modules expose some. Returns the constant names and their types for
    /// the type checker; the runtime values live in `interpret::module_constant_values`.
    pub fn module_constants(name: &str, _version: &str) -> Vec<(std::string::String, NGLType)> {
        match name {
            "example" => vec![
                ("EXAMPLE_0".to_string(), Integer),
                ("EXAMPLE_TRUE".to_string(), Bool),
                ("EXAMPLE_HELLO".to_string(), String),
            ],
            // The `batch` module (StandardModules/Batch.hs) exposes the batch array-job index as
            // an integer (`0` when not running under a batch scheduler) and a validity flag.
            "batch" => vec![
                ("JOBINDEX_OR_0".to_string(), Integer),
                ("JOBINDEX_VALID".to_string(), Bool),
            ],
            _ => Vec::new(),
        }
    }

    fn example_function() -> Function {
        let mut f = func(
            "example",
            Some(ReadSet),
            vec![],
            ReadSet,
            vec![arg("opt1", false, Symbol, vec![sym(&["test1", "test2"])])],
            vec![],
        );
        f.allows_auto_comprehension = true;
        f
    }

    /// Keyword arguments shared by `load_fastq_directory` and `load_mocat_sample`.
    fn load_directory_args() -> Vec<ArgInformation> {
        vec![
            arg("__perform_qc", false, Bool, vec![]),
            arg(
                "encoding",
                false,
                Symbol,
                vec![sym(&["auto", "33", "64", "sanger", "solexa"])],
            ),
        ]
    }

    fn load_mocat_sample_function() -> Function {
        func(
            "load_mocat_sample",
            Some(String),
            vec![],
            ReadSet,
            load_directory_args(),
            vec![],
        )
    }

    fn samtools_sort_function() -> Function {
        func(
            "samtools_sort",
            Some(MappedReadSet),
            vec![],
            MappedReadSet,
            vec![
                arg("by", false, Symbol, vec![sym(&["coordinate", "name"])]),
                // Injected by the `sortOFormat` transform in Haskell; accepted but defaulted.
                arg("__output_bam", false, Bool, vec![]),
            ],
            vec![],
        )
    }

    fn samtools_view_function() -> Function {
        func(
            "samtools_view",
            Some(MappedReadSet),
            vec![],
            MappedReadSet,
            vec![
                arg("bed_file", true, String, vec![ArgCheck::FileReadable]),
                arg("__output_bam", false, Bool, vec![]),
            ],
            vec![],
        )
    }

    fn group_args() -> Vec<ArgInformation> {
        vec![arg("name", true, String, vec![])]
    }

    fn write_args() -> Vec<ArgInformation> {
        vec![
            arg("ofile", true, String, vec![ArgCheck::FileWritable]),
            arg(
                "format",
                false,
                Symbol,
                vec![sym(&["tsv", "csv", "bam", "sam"])],
            ),
            arg(
                "format_flags",
                false,
                Symbol,
                vec![
                    ArgCheck::MinVersion(0, 7),
                    sym(&["interleaved", "always_3_fq_files"]),
                ],
            ),
            arg("verbose", false, Bool, vec![]),
            arg("comment", false, String, vec![]),
            arg(
                "auto_comments",
                false,
                list(Symbol),
                vec![sym(&["date", "script", "hash"])],
            ),
            arg(
                "compress_level",
                false,
                Integer,
                vec![ArgCheck::MinVersion(1, 5)],
            ),
        ]
    }

    fn count_args() -> Vec<ArgInformation> {
        vec![
            arg("features", false, list(String), vec![]),
            arg("subfeatures", false, list(String), vec![]),
            arg("min", false, Integer, vec![]),
            arg(
                "multiple",
                false,
                Symbol,
                vec![sym(&["all1", "dist1", "1overN", "unique_only"])],
            ),
            arg(
                "mode",
                false,
                Symbol,
                vec![sym(&[
                    "union",
                    "intersection_strict",
                    "intersection_non_empty",
                ])],
            ),
            arg("gff_file", false, String, vec![ArgCheck::FileReadable]),
            arg(
                "functional_map",
                false,
                String,
                vec![ArgCheck::FileReadable],
            ),
            arg(
                "sense",
                false,
                Symbol,
                vec![
                    sym(&["both", "sense", "antisense"]),
                    ArgCheck::MinVersion(1, 1),
                ],
            ),
            arg(
                "strand",
                false,
                Bool,
                vec![ArgCheck::Deprecated(
                    1,
                    1,
                    "Use `sense` argument instead".into(),
                )],
            ),
            arg("norm", false, Bool, vec![]),
            arg("discard_zeros", false, Bool, vec![]),
            arg("include_minus1", false, Bool, vec![]),
            arg(
                "normalization",
                false,
                Symbol,
                vec![sym(&["raw", "normed", "scaled", "fpkm"])],
            ),
            arg("reference", false, String, vec![ArgCheck::MinVersion(0, 8)]),
        ]
    }

    fn count_check_args() -> Vec<ArgInformation> {
        let mut v = count_args();
        v.push(arg("original_lno", false, Integer, vec![]));
        v
    }

    fn select_args() -> Vec<ArgInformation> {
        vec![
            arg(
                "keep_if",
                false,
                list(Symbol),
                vec![sym(&["mapped", "unmapped", "unique"])],
            ),
            arg(
                "drop_if",
                false,
                list(Symbol),
                vec![sym(&["mapped", "unmapped", "unique"])],
            ),
            arg("paired", false, Bool, vec![]),
            arg("__oname", false, String, vec![]),
        ]
    }

    fn fastq_args() -> Vec<ArgInformation> {
        vec![
            arg(
                "encoding",
                false,
                Symbol,
                vec![sym(&["auto", "33", "64", "sanger", "solexa"])],
            ),
            arg("interleaved", false, Bool, vec![ArgCheck::MinVersion(1, 1)]),
            arg("__perform_qc", false, Bool, vec![]),
        ]
    }

    fn samfile_args() -> Vec<ArgInformation> {
        vec![
            arg("name", false, String, vec![]),
            arg(
                "headers",
                false,
                String,
                vec![ArgCheck::MinVersion(0, 7), ArgCheck::FileReadable],
            ),
        ]
    }

    fn paired_args() -> Vec<ArgInformation> {
        vec![
            arg("second", true, String, vec![ArgCheck::FileReadable]),
            arg("singles", false, String, vec![ArgCheck::FileReadable]),
            arg(
                "encoding",
                false,
                Symbol,
                vec![sym(&["auto", "33", "64", "sanger", "solexa"])],
            ),
            arg("__perform_qc", false, Bool, vec![]),
        ]
    }

    fn unique_args() -> Vec<ArgInformation> {
        vec![arg("max_copies", false, Integer, vec![])]
    }

    fn preprocess_args() -> Vec<ArgInformation> {
        vec![
            arg("keep_singles", false, Bool, vec![]),
            arg("__qc_input", false, Bool, vec![]),
        ]
    }

    fn map_args() -> Vec<ArgInformation> {
        vec![
            arg("reference", false, String, vec![]),
            arg("fafile", false, String, vec![ArgCheck::FileReadable]),
            arg("mode_all", false, Bool, vec![]),
            arg("mapper", false, String, vec![]),
            arg("block_size_megabases", false, Integer, vec![]),
            arg(
                "__extra_args",
                false,
                list(String),
                vec![ArgCheck::MinVersion(1, 1)],
            ),
            arg("__oname", false, String, vec![]),
        ]
    }

    fn substrim_args() -> Vec<ArgInformation> {
        vec![arg("min_quality", true, Integer, vec![])]
    }

    fn endstrim_args() -> Vec<ArgInformation> {
        vec![
            arg("min_quality", true, Integer, vec![]),
            arg("from_ends", false, Symbol, vec![sym(&["both", "3", "5"])]),
        ]
    }

    fn smoothtrim_args() -> Vec<ArgInformation> {
        vec![
            arg("min_quality", true, Integer, vec![]),
            arg("window", true, Integer, vec![]),
        ]
    }

    fn method(
        name: &str,
        self_type: NGLType,
        arg_type: Option<NGLType>,
        return_type: NGLType,
        kwargs: Vec<ArgInformation>,
        checks: Vec<FunctionCheck>,
    ) -> MethodInfo {
        MethodInfo {
            name: MethodName(name.into()),
            self_type,
            arg_type,
            return_type,
            kwargs,
            is_pure: true,
            checks,
        }
    }

    /// The builtin methods, ported from `builtinMethods` in BuiltinFunctions.hs.
    pub fn builtin_methods() -> Vec<MethodInfo> {
        use FunctionCheck::*;
        vec![
            // NGLMappedRead
            method("flag", MappedRead, Some(Symbol), Bool, flag_args(), vec![]),
            method(
                "filter",
                MappedRead,
                None,
                MappedRead,
                filter_args(),
                vec![],
            ),
            method("pe_filter", MappedRead, None, MappedRead, vec![], vec![]),
            method("some_match", MappedRead, Some(String), Bool, vec![], vec![]),
            method("unique", MappedRead, None, MappedRead, vec![], vec![]),
            method(
                "allbest",
                MappedRead,
                None,
                MappedRead,
                vec![],
                vec![MinNGLessVersion(0, 9)],
            ),
            // NGLRead
            method("avg_quality", Read, None, Double, vec![], vec![]),
            method(
                "fraction_at_least",
                Read,
                Some(Integer),
                Double,
                vec![],
                vec![],
            ),
            method(
                "n_to_zero_quality",
                Read,
                None,
                Read,
                vec![],
                vec![MinNGLessVersion(0, 8)],
            ),
            // NGLReadSet
            method("name", ReadSet, None, String, vec![], vec![]),
            // NGLDouble / NGLInteger
            method("to_string", Double, None, String, vec![], vec![]),
            method("to_string", Integer, None, String, vec![], vec![]),
        ]
    }

    fn filter_args() -> Vec<ArgInformation> {
        vec![
            arg("min_identity_pc", false, Integer, vec![]),
            arg("min_match_size", false, Integer, vec![]),
            arg("max_trim", false, Integer, vec![ArgCheck::MinVersion(0, 7)]),
            arg("action", false, Symbol, vec![sym(&["drop", "unmatch"])]),
            arg("reverse", false, Bool, vec![]),
        ]
    }

    fn flag_args() -> Vec<ArgInformation> {
        vec![arg(
            "__0",
            false,
            Symbol,
            vec![sym(&["mapped", "unmapped"])],
        )]
    }
}
