//! "Did you mean ...?" suggestions for misspelled names, mirroring
//! `NGLess/Utils/Suggestion.hs`.
//!
//! When a user types a function/variable/argument/symbol/file name that does not exist, we try to
//! point them at the closest legal option. The matching strategy (in priority order) is:
//!
//! 1. **case-insensitive match** — same spelling, different case (ngless is case-sensitive);
//! 2. **prefix match** — what the user typed is a prefix of a legal option;
//! 3. **closest match** — Levenshtein edit distance ≤ 2.
//!
//! The first option that matches under each strategy wins, so results follow the order of the
//! `possible` list (`firstJust` in Haskell).

/// A suggestion: the text to suggest, plus a human-readable reason.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Suggestion {
    /// Suggested (legal) text.
    pub suggestion: String,
    /// Why this is being suggested (e.g. "closest match").
    pub reason: String,
}

/// Given what the user typed and the list of legal options, attempt to find a suggestion.
///
/// See also [`suggestion_message`]. Mirrors `findSuggestion`.
pub fn find_suggestion<S: AsRef<str>>(used: &str, possible: &[S]) -> Option<Suggestion> {
    let match_case = possible.iter().find_map(|s| {
        let s = s.as_ref();
        (used.to_lowercase() == s.to_lowercase()).then(|| Suggestion {
            suggestion: s.to_string(),
            reason: "note that ngless is case-sensitive".to_string(),
        })
    });
    let prefix_match = || {
        possible.iter().find_map(|s| {
            let s = s.as_ref();
            s.starts_with(used).then(|| Suggestion {
                suggestion: s.to_string(),
                reason: "prefix match".to_string(),
            })
        })
    };
    let best_match = || {
        possible.iter().find_map(|s| {
            let s = s.as_ref();
            (edit_distance(used, s) <= 2).then(|| Suggestion {
                suggestion: s.to_string(),
                reason: "closest match".to_string(),
            })
        })
    };
    match_case.or_else(prefix_match).or_else(best_match)
}

/// Like [`find_suggestion`], but returns a ready-to-append message (or the empty string if no
/// suggestion is found). Mirrors `suggestionMessage`.
pub fn suggestion_message<S: AsRef<str>>(used: &str, valid: &[S]) -> String {
    match find_suggestion(used, valid) {
        None => String::new(),
        Some(Suggestion { suggestion, reason }) => {
            format!("Did you mean '{suggestion}' ({reason})?")
        }
    }
}

/// Levenshtein edit distance between two strings, with unit costs for insertion, deletion and
/// substitution (matching `Text.EditDistance`'s `defaultEditCosts`). Operates on Unicode scalar
/// values (`char`), as Haskell's does on `Char`.
fn edit_distance(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    // Single-row dynamic programming over the edit-distance matrix.
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    let mut cur = vec![0; b.len() + 1];
    for (i, &ca) in a.iter().enumerate() {
        cur[0] = i + 1;
        for (j, &cb) in b.iter().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            cur[j + 1] = (prev[j + 1] + 1) // deletion
                .min(cur[j] + 1) // insertion
                .min(prev[j] + cost); // substitution
        }
        std::mem::swap(&mut prev, &mut cur);
    }
    prev[b.len()]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn edit_distance_basics() {
        assert_eq!(edit_distance("", ""), 0);
        assert_eq!(edit_distance("abc", "abc"), 0);
        assert_eq!(edit_distance("abc", "abd"), 1); // substitution
        assert_eq!(edit_distance("abc", "ab"), 1); // deletion
        assert_eq!(edit_distance("ab", "abc"), 1); // insertion
        assert_eq!(edit_distance("kitten", "sitting"), 3);
    }

    #[test]
    fn case_insensitive_takes_priority() {
        let opts = ["sacCer3", "hg19"];
        let s = find_suggestion("saccer3", &opts).unwrap();
        assert_eq!(s.suggestion, "sacCer3");
        assert_eq!(s.reason, "note that ngless is case-sensitive");
    }

    #[test]
    fn prefix_match() {
        let opts = ["preprocess", "process"];
        let s = find_suggestion("prepro", &opts).unwrap();
        assert_eq!(s.suggestion, "preprocess");
        assert_eq!(s.reason, "prefix match");
    }

    #[test]
    fn closest_match_within_two_edits() {
        let opts = ["reference", "fafile"];
        let s = find_suggestion("referense", &opts).unwrap();
        assert_eq!(s.suggestion, "reference");
        assert_eq!(s.reason, "closest match");
    }

    #[test]
    fn no_suggestion_when_far() {
        let opts = ["reference", "fafile"];
        assert!(find_suggestion("xyzzy", &opts).is_none());
        assert_eq!(suggestion_message("xyzzy", &opts), "");
    }

    #[test]
    fn message_format() {
        let opts = ["count"];
        assert_eq!(
            suggestion_message("conut", &opts),
            "Did you mean 'count' (closest match)?"
        );
    }
}
