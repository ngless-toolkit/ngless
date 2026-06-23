//! Transparent (de)compression for FASTQ I/O, dispatched on filename extension.
//!
//! Mirrors the behaviour of `withPossiblyCompressedFile` (read) and `moveOrCopyCompress`
//! (write) for the formats supported so far. NGLess recognises gzip (`.gz`), bzip2 (`.bz2`)
//! and zstd (`.zst`/`.zstd`); only gzip is implemented in this build, the others report a
//! clear "not supported yet" error rather than silently producing wrong output.

use std::fs::File;
use std::io::{BufWriter, Read, Write};

use flate2::read::MultiGzDecoder;
use flate2::write::GzEncoder;
use flate2::Compression;

use crate::errors::{NgError, NgErrorType, NgResult};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Compress {
    None,
    Gzip,
    Bzip2,
    Zstd,
}

/// Determine the compression of a path from its extension.
pub fn detect(path: &str) -> Compress {
    if path.ends_with(".gz") {
        Compress::Gzip
    } else if path.ends_with(".bz2") {
        Compress::Bzip2
    } else if path.ends_with(".zst") || path.ends_with(".zstd") {
        Compress::Zstd
    } else {
        Compress::None
    }
}

fn io_err(path: &str) -> impl Fn(std::io::Error) -> NgError + '_ {
    move |e| {
        NgError::new(
            NgErrorType::DataError,
            format!("Could not read {path}: {e}"),
        )
    }
}

fn unsupported(path: &str, c: Compress) -> NgError {
    let name = match c {
        Compress::Bzip2 => "bzip2",
        Compress::Zstd => "zstd",
        _ => "this compression",
    };
    NgError::script(format!(
        "{name} compression ('{path}') is not supported in this build yet."
    ))
}

/// Read a (possibly compressed) file fully into a byte vector, decompressing by extension.
pub fn read_bytes(path: &str) -> NgResult<Vec<u8>> {
    let f = File::open(path).map_err(io_err(path))?;
    let mut buf = Vec::new();
    match detect(path) {
        Compress::None => {
            let mut r = std::io::BufReader::new(f);
            r.read_to_end(&mut buf).map_err(io_err(path))?;
        }
        Compress::Gzip => {
            let mut r = MultiGzDecoder::new(f);
            r.read_to_end(&mut buf).map_err(io_err(path))?;
        }
        other => return Err(unsupported(path, other)),
    }
    Ok(buf)
}

/// Read a (possibly compressed) file fully into a `String`.
pub fn read_to_string(path: &str) -> NgResult<String> {
    let bytes = read_bytes(path)?;
    String::from_utf8(bytes).map_err(|e| {
        NgError::new(
            NgErrorType::DataError,
            format!("File {path} is not valid UTF-8: {e}"),
        )
    })
}

/// Write bytes to `path`, compressing by extension.
pub fn write_bytes(path: &str, data: &[u8]) -> NgResult<()> {
    let write_err = |e: std::io::Error| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not write {path}: {e}"),
        )
    };
    let f = File::create(path).map_err(write_err)?;
    match detect(path) {
        Compress::None => {
            let mut w = BufWriter::new(f);
            w.write_all(data).map_err(write_err)?;
            w.flush().map_err(write_err)?;
        }
        Compress::Gzip => {
            let mut w = GzEncoder::new(BufWriter::new(f), Compression::default());
            w.write_all(data).map_err(write_err)?;
            w.finish().map_err(write_err)?;
        }
        other => return Err(unsupported(path, other)),
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detect_extensions() {
        assert_eq!(detect("a.fq"), Compress::None);
        assert_eq!(detect("a.fq.gz"), Compress::Gzip);
        assert_eq!(detect("a.fq.bz2"), Compress::Bzip2);
        assert_eq!(detect("a.fq.zst"), Compress::Zstd);
        assert_eq!(detect("a.fq.zstd"), Compress::Zstd);
    }

    #[test]
    fn gzip_round_trip() {
        let dir = std::env::temp_dir();
        let p = dir.join(format!("ngless_comp_{}.fq.gz", std::process::id()));
        let ps = p.to_string_lossy().to_string();
        let payload = "@r\nACGT\n+\nIIII\n";
        write_bytes(&ps, payload.as_bytes()).unwrap();
        // The on-disk file is actually gzip-compressed (different bytes)...
        let raw = std::fs::read(&p).unwrap();
        assert_ne!(raw, payload.as_bytes());
        // ...but reads back transparently.
        assert_eq!(read_to_string(&ps).unwrap(), payload);
        let _ = std::fs::remove_file(&p);
    }

    #[test]
    fn unsupported_formats_error() {
        assert!(read_bytes("nonexistent.fq.bz2").is_err());
    }
}
