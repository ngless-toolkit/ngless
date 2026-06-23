//! Module / function metadata, mirroring `NGLess/Modules.hs` and the builtin tables in
//! `NGLess/BuiltinFunctions.hs`.
//!
//! Only what the front end needs is ported: the builtin function and method signatures used
//! by type checking and validation. Module loading, references and `runFunction` are deferred
//! to later milestones.

use crate::ast::{FuncName, MethodName, NGLType};

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

pub use builders::{builtin_functions, builtin_methods};

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
                "__merge_samfiles",
                Some(list(String)),
                vec![],
                MappedReadSet,
                vec![],
                vec![],
            ),
        ]
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
