//! Transparent (de)compression for FASTQ/SAM I/O, dispatched on filename extension.
//!
//! Mirrors the behaviour of `withPossiblyCompressedFile` (read) and `moveOrCopyCompress`
//! (write). NGLess recognises gzip (`.gz`), bzip2 (`.bz2`) and zstd (`.zst`/`.zstd`); all four
//! states (including uncompressed) are handled here. Output is content-equivalent to NGLess —
//! the exact compressed bytes need not match, since the test suite compares decompressed data.

use std::fs::File;
use std::io::{BufWriter, Read, Write};

use flate2::read::MultiGzDecoder;
use flate2::write::GzEncoder;
use flate2::Compression;

use crate::errors::{NgError, NgErrorType, NgResult};

/// zstd compression level used for output (NGLess commonly uses level 3).
const ZSTD_LEVEL: i32 = 3;

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
        Compress::Bzip2 => {
            let mut r = bzip2::read::MultiBzDecoder::new(f);
            r.read_to_end(&mut buf).map_err(io_err(path))?;
        }
        // `decode_all` consumes the whole stream, including concatenated frames.
        Compress::Zstd => {
            buf = zstd::stream::decode_all(f).map_err(io_err(path))?;
        }
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
        Compress::Bzip2 => {
            let mut w =
                bzip2::write::BzEncoder::new(BufWriter::new(f), bzip2::Compression::default());
            w.write_all(data).map_err(write_err)?;
            w.finish().map_err(write_err)?;
        }
        Compress::Zstd => {
            let compressed = zstd::stream::encode_all(data, ZSTD_LEVEL).map_err(write_err)?;
            let mut w = BufWriter::new(f);
            w.write_all(&compressed).map_err(write_err)?;
            w.flush().map_err(write_err)?;
        }
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
    fn missing_file_errors() {
        assert!(read_bytes("nonexistent.fq.bz2").is_err());
        assert!(read_bytes("nonexistent.fq.zst").is_err());
    }

    fn round_trip(ext: &str) {
        let dir = std::env::temp_dir();
        let p = dir.join(format!("ngless_comp_{}_{}.{ext}", std::process::id(), ext));
        let ps = p.to_string_lossy().to_string();
        let payload = "@r\nACGTACGTACGT\n+\nIIIIIIIIIIII\n";
        write_bytes(&ps, payload.as_bytes()).unwrap();
        // The on-disk bytes are actually compressed, not the plain payload...
        assert_ne!(std::fs::read(&p).unwrap(), payload.as_bytes());
        // ...but read back transparently.
        assert_eq!(read_to_string(&ps).unwrap(), payload);
        let _ = std::fs::remove_file(&p);
    }

    #[test]
    fn bzip2_round_trip() {
        round_trip("bz2");
    }

    #[test]
    fn zstd_round_trip() {
        round_trip("zst");
    }
}
