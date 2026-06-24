//! Citation collection and the run header, mirroring `NGLess/Citations.hs` and `printHeader`
//! in `Execs/Main.hs`. The header lists the base NGLess citation plus any citations contributed
//! by imported modules or by citation-bearing functions used in the script (`map`/`assemble`/
//! `orf_find`).

use crate::ast::{Expression, FuncName, Header, Script};
use crate::version::VERSION_STR;
use std::collections::BTreeSet;

/// The base NGLess citation, always emitted first (mirrors `nglessCitation`).
const NGLESS_CITATION: &str = "Coelho, L.P., Alves, R., Monteiro, P., Huerta-Cepas, J., Freitas, A.T., and Bork, P., NG-meta-profiler: fast processing of metagenomes using NGLess, a domain-specific language. in Microbiome 7:84 (2019). DOI: https://doi.org/10.1186/s40168-019-0684-8";

/// Per-function citations (mirrors the `citations` association list in `Citations.hs`).
fn function_citation(name: &str) -> Option<&'static str> {
    match name {
        "assemble" => Some("Li, D., Liu, C.M., Luo, R., Sadakane, K. and Lam, T.W., 2015. MEGAHIT: an ultra-fast single-node solution for large and complex metagenomics assembly via succinct de Bruijn graph. Bioinformatics, 31(10), pp.1674-1676."),
        "orf_find" => Some("Hyatt, D., Chen, G.L., LoCascio, P.F., Land, M.L., Larimer, F.W. and Hauser, L.J., 2010. Prodigal: prokaryotic gene recognition and translation initiation site identification. BMC bioinformatics, 11(1), p.119."),
        "map" => Some("Li, H., 2013. Aligning sequence reads, clone sequences and assembly contigs with BWA-MEM. arXiv preprint arXiv:1303.3997."),
        _ => None,
    }
}

/// Citations contributed by an imported module (mirrors `modCitations` for the standard modules).
fn module_citations(name: &str) -> &'static [&'static str] {
    match name {
        "samtools" => &["'The Sequence Alignment/Map format and SAMtools' by Heng Li, Bob Handsaker, Alec Wysoker, Tim Fennell, Jue Ruan, Nils Homer, Gabor Marth, Goncalo Abecasis, Richard Durbin, and 1000 Genome Project Data Processing Subgroup in Bioinformatics (2009) 25 (16): 2078-2079 (2009) DOI:10.1093/bioinformatics/btp352"],
        "minimap2" => &["Li, H. (2018). Minimap2: pairwise alignment for nucleotide sequences. Bioinformatics, 34:3094-3100. doi:10.1093/bioinformatics/bty191"],
        _ => &[],
    }
}

/// Collect the citations to print for a script (mirrors `collectCitations`). The base citation
/// comes first, followed by the sorted, deduplicated set of module and per-function citations.
pub fn collect_citations(script: &Script) -> Vec<String> {
    let mut rest: BTreeSet<&'static str> = BTreeSet::new();

    if let Some(Header { modules, .. }) = &script.header {
        for m in modules {
            for c in module_citations(m.name()) {
                rest.insert(c);
            }
        }
    }

    // Only assignments wrapping a function call contribute a citation (mirrors the pattern
    // match in `collectCitations`).
    for (_, expr) in &script.body {
        if let Expression::Assignment(_, inner) = expr {
            if let Expression::FunctionCall(FuncName(f), ..) = inner.as_ref() {
                if let Some(c) = function_citation(f) {
                    rest.insert(c);
                }
            }
        }
    }

    let mut out = vec![NGLESS_CITATION.to_string()];
    out.extend(rest.into_iter().map(|s| s.to_string()));
    out
}

/// Format a single citation, wrapping at 90 columns with a leading tab (mirrors `formatCitation`).
/// Each returned line is `"\t"` followed by a space and the space-joined words; the lines are
/// joined with newlines and the result has no trailing newline.
fn format_citation(citation: &str) -> String {
    const LINE_MAX: usize = 90;
    let mut words: Vec<&str> = Vec::new();
    words.push("-");
    words.extend(citation.split_whitespace());

    let mut lines: Vec<String> = Vec::new();
    let mut current: Vec<&str> = Vec::new();
    let mut n = 0usize;
    for w in words {
        if current.is_empty() {
            current.push(w);
            n = 2 + w.len();
        } else if w.len() + n > LINE_MAX {
            lines.push(format!("\t {}", current.join(" ")));
            current.clear();
            current.push(w);
            n = 2 + w.len();
        } else {
            n += w.len();
            current.push(w);
        }
    }
    if !current.is_empty() {
        lines.push(format!("\t {}", current.join(" ")));
    }
    lines.join("\n")
}

/// Print the run header to stdout (mirrors `printHeader`). Emits the version/copyright banner and,
/// when there are citations, the "please cite" block.
pub fn print_header(citations: &[String]) {
    print!("NGLess v{VERSION_STR} (C) NGLess authors\nhttps://ngless.embl.de/\n\n");
    if !citations.is_empty() {
        print!("When publishing results from this script, please cite the following references:\n\n");
        for c in citations {
            // `putStrLn (formatCitation c)` — formatCitation has a trailing newline (T.unlines),
            // then putStrLn adds another, giving a blank line between citations.
            println!("{}\n", format_citation(c));
        }
        print!("\n");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn formats_base_citation_with_three_wrapped_lines() {
        let formatted = format_citation(NGLESS_CITATION);
        let expected = "\t - Coelho, L.P., Alves, R., Monteiro, P., Huerta-Cepas, J., Freitas, A.T., and Bork, P.,\n\
                        \t NG-meta-profiler: fast processing of metagenomes using NGLess, a domain-specific language. in\n\
                        \t Microbiome 7:84 (2019). DOI: https://doi.org/10.1186/s40168-019-0684-8";
        assert_eq!(formatted, expected);
    }
}
