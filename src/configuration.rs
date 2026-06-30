//! NGLess configuration files, mirroring `NGLess/Configuration.hs`.
//!
//! Configuration is built in three steps (`initConfiguration`):
//!
//! 1. [`guess_configuration`] sets defaults from the environment (`guessConfiguration`).
//! 2. [`read_config_files`] layers the config files on top (`readConfigFiles`).
//! 3. the command line overrides the result (done in [`crate::cli`], mirroring
//!    `updateConfigurationOpts`).
//!
//! The file format is the subset of the Haskell `Data.Configurator` format that real ngless config
//! files use: `#` line comments and `key = value` bindings where a value is a double-quoted string,
//! a bare word, a `true`/`false` boolean, or a `[..]` list of strings. Interpolation, `import`
//! directives, numeric literals and nested groups are not supported (ngless reads none of those
//! keys). Unsupported value forms are read leniently as bare strings rather than rejected.
//!
//! Only the keys ngless actually reads are honoured (the `jobs` key, despite the docs, is *not*
//! read from the config file by the Haskell binary — `nThreads` comes from the command line only).

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::OnceLock;

use crate::errors::{NgError, NgErrorType, NgResult};
use crate::output::ColorSetting;

/// The default download base URL (mirrors `defaultBaseURL` in `Configuration.hs`).
pub const DEFAULT_BASE_URL: &str = "https://ngless-resources.big-data-biology.org/";

/// Resolved ngless configuration (the subset of `NGLessConfiguration` that the config file feeds).
/// Fields that are only ever set from the command line (verbosity, trace, subsample, argv, ...)
/// live in [`crate::cli`]'s `RunOpts` and are not duplicated here.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Configuration {
    /// `download-url`: base URL for reference/demo downloads (`nConfDownloadBaseURL`).
    pub download_base_url: String,
    /// `global-data-directory`: global (root) data directory (`nConfGlobalDataDirectory`).
    pub global_data_directory: String,
    /// `user-directory`: user-writable cache directory (`nConfUserDirectory`).
    pub user_directory: String,
    /// `user-data-directory`: user-writable data directory (`nConfUserDataDirectory`).
    pub user_data_directory: String,
    /// `temporary-directory`: where intermediate files are written (`nConfTemporaryDirectory`).
    pub temporary_directory: String,
    /// `keep-temporary-files`: keep intermediates at end of run (`nConfKeepTemporaryFiles`).
    pub keep_temporary_files: bool,
    /// `strict-threads`: never exceed the requested thread count (`nConfStrictThreads`).
    pub strict_threads: bool,
    /// `create-report`: write the HTML/JS run report (`nConfCreateReportDirectory`).
    pub create_report_directory: bool,
    /// `color`: colour-output setting (`nConfColor`).
    pub color: ColorSetting,
    /// `print-header`: print the version/citation banner (`nConfPrintHeader`).
    pub print_header: bool,
    /// `search-path`: `<...>` search-path expansion roots (`nConfSearchPath`).
    pub search_path: Vec<String>,
    /// `index-path`: where to store mapper indices (`nConfIndexStorePath`).
    pub index_store_path: Option<String>,
}

/// Defaults from the environment, mirroring `guessConfiguration`.
pub fn guess_configuration() -> Configuration {
    let user_directory = default_user_ngless_directory();
    let global_data_directory = global_data_directory_default();
    let user_data_directory = join(&user_directory, "data");
    Configuration {
        download_base_url: DEFAULT_BASE_URL.to_string(),
        global_data_directory,
        user_data_directory,
        user_directory,
        temporary_directory: std::env::temp_dir().to_string_lossy().into_owned(),
        keep_temporary_files: false,
        strict_threads: false,
        create_report_directory: true,
        color: ColorSetting::Auto,
        print_header: true,
        search_path: Vec::new(),
        index_store_path: None,
    }
}

/// The default user ngless directory (mirrors `getDefaultUserNglessDirectory`): `$XDG_DATA_HOME/ngless`
/// if `XDG_DATA_HOME` is set, otherwise `$HOME/.local/share/ngless`.
fn default_user_ngless_directory() -> String {
    if let Some(xdg) = std::env::var_os("XDG_DATA_HOME") {
        if !xdg.is_empty() {
            return join(&xdg.to_string_lossy(), "ngless");
        }
    }
    let home = std::env::var("HOME").unwrap_or_default();
    join(&home, ".local/share/ngless")
}

/// `<binary-dir>/../share/ngless/data` (mirrors `nglessBinDirectory </> "../share/ngless/data"`).
fn global_data_directory_default() -> String {
    let bindir = std::env::current_exe()
        .ok()
        .and_then(|exe| exe.parent().map(PathBuf::from))
        .unwrap_or_default();
    join(&bindir.to_string_lossy(), "../share/ngless/data")
}

/// Join two path fragments with a single `/`, like Haskell's `</>`. `b` absolute replaces `a`.
fn join(a: &str, b: &str) -> String {
    if b.starts_with('/') {
        return b.to_string();
    }
    if a.is_empty() {
        return b.to_string();
    }
    if a.ends_with('/') {
        format!("{a}{b}")
    } else {
        format!("{a}/{b}")
    }
}

/// Build the configuration from defaults plus config files, mirroring `readConfigFiles`.
///
/// The default search locations (all optional, parsed in order so a later file overrides an
/// earlier one) are `$HOME/.config/ngless.conf`, `$HOME/.ngless.conf` and `/etc/ngless.conf`; the
/// `cli_files` (from `-c/--config-file`, required to exist) are applied last and so take precedence.
pub fn read_config_files(cli_files: &[String]) -> NgResult<Configuration> {
    let mut config = guess_configuration();
    let home = std::env::var("HOME").unwrap_or_default();
    let mut files: Vec<(String, bool)> = Vec::new();
    if !home.is_empty() {
        files.push((join(&home, ".config/ngless.conf"), false));
        files.push((join(&home, ".ngless.conf"), false));
    }
    files.push(("/etc/ngless.conf".to_string(), false));
    for f in cli_files {
        files.push((f.clone(), true));
    }

    for (path, required) in &files {
        match std::fs::read_to_string(path) {
            Ok(text) => {
                let parsed = parse_config_text(&text).map_err(|e| {
                    NgError::script(format!("Error parsing config file {path}: {e}"))
                })?;
                apply(&mut config, &parsed, path)?;
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                if *required {
                    return Err(NgError::new(
                        NgErrorType::SystemError,
                        format!("Required configuration file '{path}' does not exist."),
                    ));
                }
            }
            Err(e) => {
                return Err(NgError::new(
                    NgErrorType::SystemError,
                    format!("Could not read configuration file '{path}': {e}"),
                ));
            }
        }
    }
    Ok(config)
}

/// A parsed config value: the subset of `Data.Configurator`'s `Value` ngless needs.
#[derive(Clone, Debug, PartialEq, Eq)]
enum CValue {
    Bool(bool),
    Str(String),
    List(Vec<String>),
}

/// Overlay the parsed key/values onto `config`, mirroring the `lookupDefault`/`lookup` block of
/// `readConfigFiles`. Unknown keys are ignored (Haskell only reads the keys it knows).
fn apply(config: &mut Configuration, parsed: &HashMap<String, CValue>, path: &str) -> NgResult<()> {
    for (key, value) in parsed {
        match key.as_str() {
            "download-url" => config.download_base_url = as_string(value, key, path)?,
            "global-data-directory" => config.global_data_directory = as_string(value, key, path)?,
            "user-directory" => config.user_directory = as_string(value, key, path)?,
            "user-data-directory" => config.user_data_directory = as_string(value, key, path)?,
            "temporary-directory" => config.temporary_directory = as_string(value, key, path)?,
            "keep-temporary-files" => config.keep_temporary_files = as_bool(value, key, path)?,
            "strict-threads" => config.strict_threads = as_bool(value, key, path)?,
            "color" => config.color = as_color(value, key, path)?,
            "print-header" => config.print_header = as_bool(value, key, path)?,
            "create-report" => config.create_report_directory = as_bool(value, key, path)?,
            "search-path" => config.search_path = as_string_list(value),
            "index-path" => config.index_store_path = Some(as_string(value, key, path)?),
            _ => {}
        }
    }
    Ok(())
}

fn type_error(key: &str, path: &str, expected: &str) -> NgError {
    NgError::script(format!(
        "Configuration key '{key}' in '{path}' could not be read as {expected}."
    ))
}

fn as_string(value: &CValue, key: &str, path: &str) -> NgResult<String> {
    match value {
        CValue::Str(s) => Ok(s.clone()),
        CValue::Bool(b) => Ok(b.to_string()),
        CValue::List(_) => Err(type_error(key, path, "a string")),
    }
}

fn as_bool(value: &CValue, key: &str, path: &str) -> NgResult<bool> {
    match value {
        CValue::Bool(b) => Ok(*b),
        _ => Err(type_error(key, path, "a boolean (true/false)")),
    }
}

/// A search-path may be a list or a single string (Haskell expects a list; a bare string is
/// accepted leniently and wrapped in a singleton list).
fn as_string_list(value: &CValue) -> Vec<String> {
    match value {
        CValue::List(xs) => xs.clone(),
        CValue::Str(s) => vec![s.clone()],
        CValue::Bool(b) => vec![b.to_string()],
    }
}

/// Map a `color` value to a [`ColorSetting`] (mirrors the `Configured ColorSetting` instance:
/// `auto`/`force`/`none`; `yes`/`no` are accepted as the documented synonyms).
fn as_color(value: &CValue, key: &str, path: &str) -> NgResult<ColorSetting> {
    let s = match value {
        CValue::Str(s) => s.as_str(),
        _ => return Err(type_error(key, path, "a colour setting")),
    };
    match s {
        "auto" => Ok(ColorSetting::Auto),
        "force" | "yes" => Ok(ColorSetting::Force),
        "none" | "no" => Ok(ColorSetting::No),
        other => Err(NgError::script(format!(
            "Configuration key 'color' in '{path}' has unknown value '{other}' \
             (expected 'auto', 'force' or 'none')."
        ))),
    }
}

/// Parse a config file's text into a flat key/value map.
fn parse_config_text(text: &str) -> Result<HashMap<String, CValue>, String> {
    let mut out = HashMap::new();
    for (lno, raw) in text.lines().enumerate() {
        let line = strip_comment(raw).trim();
        if line.is_empty() {
            continue;
        }
        let eq = line
            .find('=')
            .ok_or_else(|| format!("line {}: expected 'key = value'", lno + 1))?;
        let key = line[..eq].trim();
        let value = line[eq + 1..].trim();
        if key.is_empty() {
            return Err(format!("line {}: empty key", lno + 1));
        }
        let parsed = parse_value(value).map_err(|e| format!("line {}: {e}", lno + 1))?;
        out.insert(key.to_string(), parsed);
    }
    Ok(out)
}

/// Drop a trailing `#` comment, ignoring `#` inside a double-quoted string.
fn strip_comment(line: &str) -> &str {
    let mut in_quote = false;
    let mut escaped = false;
    for (i, c) in line.char_indices() {
        if in_quote {
            if escaped {
                escaped = false;
            } else if c == '\\' {
                escaped = true;
            } else if c == '"' {
                in_quote = false;
            }
        } else if c == '#' {
            return &line[..i];
        } else if c == '"' {
            in_quote = true;
        }
    }
    line
}

fn parse_value(s: &str) -> Result<CValue, String> {
    let s = s.trim();
    if let Some(inner) = s.strip_prefix('[').and_then(|x| x.strip_suffix(']')) {
        let mut items = Vec::new();
        for part in split_top_commas(inner) {
            let part = part.trim();
            if part.is_empty() {
                continue;
            }
            items.push(parse_scalar_string(part)?);
        }
        return Ok(CValue::List(items));
    }
    if s.starts_with('"') {
        return Ok(CValue::Str(parse_quoted(s)?));
    }
    match s {
        // Only `true`/`false` are booleans (matching `Data.Configurator`); everything else bare
        // (e.g. `auto`, an unquoted path) is read as a string.
        "true" => Ok(CValue::Bool(true)),
        "false" => Ok(CValue::Bool(false)),
        other => Ok(CValue::Str(other.to_string())),
    }
}

/// Parse a list element as a string (quoted or bare).
fn parse_scalar_string(s: &str) -> Result<String, String> {
    if s.starts_with('"') {
        parse_quoted(s)
    } else {
        Ok(s.to_string())
    }
}

/// Parse a double-quoted string with `\\`, `\"`, `\n`, `\t`, `\r` escapes.
fn parse_quoted(s: &str) -> Result<String, String> {
    let bytes: Vec<char> = s.chars().collect();
    if bytes.first() != Some(&'"') {
        return Err("expected a quoted string".to_string());
    }
    let mut out = String::new();
    let mut i = 1;
    while i < bytes.len() {
        let c = bytes[i];
        if c == '\\' {
            i += 1;
            match bytes.get(i) {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some('"') => out.push('"'),
                Some('\\') => out.push('\\'),
                Some(other) => out.push(*other),
                None => return Err("unterminated escape".to_string()),
            }
            i += 1;
        } else if c == '"' {
            if i != bytes.len() - 1 {
                return Err("unexpected trailing characters after closing quote".to_string());
            }
            return Ok(out);
        } else {
            out.push(c);
            i += 1;
        }
    }
    Err("unterminated string".to_string())
}

/// Split on commas that are not inside a double-quoted string.
fn split_top_commas(s: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut in_quote = false;
    let mut escaped = false;
    let mut start = 0;
    for (i, c) in s.char_indices() {
        if in_quote {
            if escaped {
                escaped = false;
            } else if c == '\\' {
                escaped = true;
            } else if c == '"' {
                in_quote = false;
            }
        } else if c == '"' {
            in_quote = true;
        } else if c == ',' {
            parts.push(&s[start..i]);
            start = i + 1;
        }
    }
    parts.push(&s[start..]);
    parts
}

static GLOBAL: OnceLock<Configuration> = OnceLock::new();

/// Install the process-global configuration. Called once at startup (after CLI overrides have been
/// applied), mirroring the single global `nglConfiguration` `IORef`. Later calls are ignored.
pub fn init_global(config: Configuration) {
    let _ = GLOBAL.set(config);
}

/// The process-global configuration. Callers reached before [`init_global`] (e.g. unit tests) get
/// the environment-guessed defaults, matching `guessConfiguration`.
pub fn global() -> &'static Configuration {
    GLOBAL.get_or_init(guess_configuration)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(text: &str) -> HashMap<String, CValue> {
        parse_config_text(text).unwrap()
    }

    #[test]
    fn basic_bindings() {
        let m = parse(
            "temporary-directory = \"/local/ngless-temp/\"\n\
             keep-temporary-files = true\n\
             strict-threads = false\n",
        );
        assert_eq!(
            m.get("temporary-directory"),
            Some(&CValue::Str("/local/ngless-temp/".to_string()))
        );
        assert_eq!(m.get("keep-temporary-files"), Some(&CValue::Bool(true)));
        assert_eq!(m.get("strict-threads"), Some(&CValue::Bool(false)));
    }

    #[test]
    fn comments_and_blanks() {
        let m = parse(
            "# a comment\n\
             \n\
             download-url = \"https://example.org/\"  # trailing comment\n",
        );
        assert_eq!(m.len(), 1);
        assert_eq!(
            m.get("download-url"),
            Some(&CValue::Str("https://example.org/".to_string()))
        );
    }

    #[test]
    fn hash_inside_quotes_is_kept() {
        let m = parse("download-url = \"https://example.org/#frag\"\n");
        assert_eq!(
            m.get("download-url"),
            Some(&CValue::Str("https://example.org/#frag".to_string()))
        );
    }

    #[test]
    fn list_value() {
        let m = parse("search-path = [\"/a/b\", \"/c/d\"]\n");
        assert_eq!(
            m.get("search-path"),
            Some(&CValue::List(vec!["/a/b".to_string(), "/c/d".to_string()]))
        );
    }

    #[test]
    fn bare_string_is_lenient() {
        let m = parse("color = auto\n");
        assert_eq!(m.get("color"), Some(&CValue::Str("auto".to_string())));
    }

    #[test]
    fn apply_overlays_known_keys() {
        let mut config = guess_configuration();
        let parsed = parse(
            "keep-temporary-files = true\n\
             color = \"force\"\n\
             search-path = [\"/refs\"]\n\
             unknown-key = \"ignored\"\n",
        );
        apply(&mut config, &parsed, "test.conf").unwrap();
        assert!(config.keep_temporary_files);
        assert_eq!(config.color, ColorSetting::Force);
        assert_eq!(config.search_path, vec!["/refs".to_string()]);
    }

    #[test]
    fn apply_rejects_bad_bool() {
        let mut config = guess_configuration();
        let parsed = parse("keep-temporary-files = \"sometimes\"\n");
        assert!(apply(&mut config, &parsed, "test.conf").is_err());
    }

    #[test]
    fn color_synonyms() {
        for (text, expected) in [
            ("color = \"auto\"\n", ColorSetting::Auto),
            ("color = \"force\"\n", ColorSetting::Force),
            ("color = \"yes\"\n", ColorSetting::Force),
            ("color = \"none\"\n", ColorSetting::No),
            ("color = \"no\"\n", ColorSetting::No),
        ] {
            let mut config = guess_configuration();
            apply(&mut config, &parse(text), "t.conf").unwrap();
            assert_eq!(config.color, expected);
        }
    }
}
