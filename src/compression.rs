//! Transparent (de)compression for FASTQ/SAM I/O, dispatched on filename extension.
//!
//! Mirrors the behaviour of `withPossiblyCompressedFile` (read) and `moveOrCopyCompress`
//! (write). NGLess recognises gzip (`.gz`), bzip2 (`.bz2`) and zstd (`.zst`/`.zstd`); all four
//! states (including uncompressed) are handled here. Output is content-equivalent to NGLess —
//! the exact compressed bytes need not match, since the test suite compares decompressed data.

use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Read, Write};

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
    let f = File::create(path).map_err(|e| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not write {path}: {e}"),
        )
    })?;
    let f = write_all_compressed(f, detect(path), path, data)?;
    drop(f);
    Ok(())
}

/// Write `data` to `path` **atomically and durably** (mirrors `moveOrCopyCompress` with
/// `syncFile`): the bytes are compressed (per `path`'s extension) into a temporary sibling file,
/// `fsync`ed, then `rename`d onto `path`. A reader therefore never observes a half-written file,
/// and two processes racing to write the same `path` cannot leave a torn result — the rename is
/// atomic on a POSIX filesystem. Used by `collect()` for its partial and final outputs.
pub fn write_bytes_atomic(path: &str, data: &[u8]) -> NgResult<()> {
    let sys_err = |e: std::io::Error| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not write {path}: {e}"),
        )
    };
    // A temp sibling in the same directory, so the final rename stays on one filesystem.
    let tmp = format!(
        "{path}.tmp.{}.{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0)
    );
    let f = File::create(&tmp).map_err(sys_err)?;
    // Compression keys off the *final* path's extension, not the temp name.
    let synced = (|| {
        let f = write_all_compressed(f, detect(path), path, data)?;
        f.sync_all().map_err(sys_err)?;
        Ok(())
    })();
    if let Err(e) = synced {
        let _ = std::fs::remove_file(&tmp);
        return Err(e);
    }
    std::fs::rename(&tmp, path).map_err(|e| {
        let _ = std::fs::remove_file(&tmp);
        sys_err(e)
    })?;
    Ok(())
}

/// Write `data` to the already-open file `f`, compressing per `compress` (mirrors the body of
/// [`write_bytes`]). Returns the underlying `File` (with all data flushed to it) so the caller can
/// `fsync` and/or rename it.
fn write_all_compressed(
    f: File,
    compress: Compress,
    err_path: &str,
    data: &[u8],
) -> NgResult<File> {
    let write_err = |e: std::io::Error| {
        NgError::new(
            NgErrorType::SystemError,
            format!("Could not write {err_path}: {e}"),
        )
    };
    let into_inner_err = |e: std::io::IntoInnerError<BufWriter<File>>| write_err(e.into_error());
    match compress {
        Compress::None => {
            let mut w = BufWriter::new(f);
            w.write_all(data).map_err(write_err)?;
            w.into_inner().map_err(into_inner_err)
        }
        Compress::Gzip => {
            let mut w = GzEncoder::new(BufWriter::new(f), Compression::default());
            w.write_all(data).map_err(write_err)?;
            let bw = w.finish().map_err(write_err)?;
            bw.into_inner().map_err(into_inner_err)
        }
        Compress::Bzip2 => {
            let mut w =
                bzip2::write::BzEncoder::new(BufWriter::new(f), bzip2::Compression::default());
            w.write_all(data).map_err(write_err)?;
            let bw = w.finish().map_err(write_err)?;
            bw.into_inner().map_err(into_inner_err)
        }
        Compress::Zstd => {
            let compressed = zstd::stream::encode_all(data, ZSTD_LEVEL).map_err(write_err)?;
            let mut w = BufWriter::new(f);
            w.write_all(&compressed).map_err(write_err)?;
            w.into_inner().map_err(into_inner_err)
        }
    }
}

/// Open a (possibly compressed) file for streaming, bounded-memory reads, decompressing by
/// extension. The decoders match [`read_bytes`] exactly (`MultiGzDecoder`, `MultiBzDecoder`,
/// and a multi-frame zstd `read::Decoder`), so streamed bytes equal the whole-file read.
///
/// Content-agnostic (no FASTQ/SAM knowledge): the FASTQ record iterator and the SAM line
/// reader both build on top of this.
pub fn open_read(path: &str) -> NgResult<Box<dyn BufRead>> {
    let f = File::open(path).map_err(io_err(path))?;
    Ok(match detect(path) {
        Compress::None => Box::new(BufReader::new(f)),
        Compress::Gzip => Box::new(BufReader::new(MultiGzDecoder::new(f))),
        Compress::Bzip2 => Box::new(BufReader::new(bzip2::read::MultiBzDecoder::new(f))),
        // `read::Decoder::new` concatenates frames until EOF by default, matching `decode_all`.
        Compress::Zstd => Box::new(BufReader::new(
            zstd::stream::read::Decoder::new(f).map_err(io_err(path))?,
        )),
    })
}

/// A streaming, by-extension compressing sink over a file, mirroring [`write_bytes`]'s
/// compression choices but writing incrementally (bounded memory). Content-agnostic.
///
/// This is an enum over the concrete encoders rather than a `Box<dyn Write>` because
/// `GzEncoder`/`BzEncoder`/zstd `Encoder` finalize via a `finish(self)` that consumes the
/// concrete type — unreachable through a trait object. [`StreamWriter::finish`] is mandatory:
/// dropping a gzip/bzip2/zstd writer without finishing truncates the stream.
enum WriterInner {
    Plain(BufWriter<File>),
    Gzip(GzEncoder<BufWriter<File>>),
    Bzip2(bzip2::write::BzEncoder<BufWriter<File>>),
    Zstd(zstd::stream::write::Encoder<'static, BufWriter<File>>),
}

pub struct StreamWriter {
    inner: WriterInner,
    path: String,
}

impl StreamWriter {
    /// Create a writer for `path`, choosing the compressor by extension. Levels replicate
    /// [`write_bytes`]: gzip/bzip2 `Compression::default()`, zstd [`ZSTD_LEVEL`].
    pub fn create(path: &str) -> NgResult<StreamWriter> {
        let f = File::create(path).map_err(|e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write {path}: {e}"),
            )
        })?;
        let inner = match detect(path) {
            Compress::None => WriterInner::Plain(BufWriter::new(f)),
            Compress::Gzip => {
                WriterInner::Gzip(GzEncoder::new(BufWriter::new(f), Compression::default()))
            }
            Compress::Bzip2 => WriterInner::Bzip2(bzip2::write::BzEncoder::new(
                BufWriter::new(f),
                bzip2::Compression::default(),
            )),
            Compress::Zstd => WriterInner::Zstd(
                zstd::stream::write::Encoder::new(BufWriter::new(f), ZSTD_LEVEL).map_err(|e| {
                    NgError::new(
                        NgErrorType::SystemError,
                        format!("Could not write {path}: {e}"),
                    )
                })?,
            ),
        };
        Ok(StreamWriter {
            inner,
            path: path.to_string(),
        })
    }

    fn write_err(&self) -> impl Fn(std::io::Error) -> NgError + '_ {
        move |e| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write {}: {e}", self.path),
            )
        }
    }

    /// Write `line` followed by a single `\n` (generic; no FASTQ semantics).
    pub fn write_line(&mut self, line: &[u8]) -> NgResult<()> {
        self.write_all(line).map_err(self.write_err())?;
        self.write_all(b"\n").map_err(self.write_err())?;
        Ok(())
    }

    /// Write raw bytes verbatim (no added newline). Used for pre-encoded records that already
    /// carry their own line terminators (e.g. a `fq_encode`d FASTQ record).
    pub fn write_chunk(&mut self, buf: &[u8]) -> NgResult<()> {
        self.write_all(buf).map_err(self.write_err())?;
        Ok(())
    }

    /// Finalize the stream: flush/finish the encoder and the underlying buffered file. Must be
    /// called — for gzip/bzip2/zstd, dropping without finishing leaves a truncated file.
    pub fn finish(self) -> NgResult<()> {
        let path = self.path;
        let werr = |e: std::io::Error| {
            NgError::new(
                NgErrorType::SystemError,
                format!("Could not write {path}: {e}"),
            )
        };
        match self.inner {
            WriterInner::Plain(mut w) => {
                w.flush().map_err(werr)?;
            }
            WriterInner::Gzip(w) => {
                w.finish().map_err(werr)?.flush().map_err(werr)?;
            }
            WriterInner::Bzip2(w) => {
                w.finish().map_err(werr)?.flush().map_err(werr)?;
            }
            WriterInner::Zstd(w) => {
                w.finish().map_err(werr)?.flush().map_err(werr)?;
            }
        }
        Ok(())
    }
}

impl Write for StreamWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match &mut self.inner {
            WriterInner::Plain(w) => w.write(buf),
            WriterInner::Gzip(w) => w.write(buf),
            WriterInner::Bzip2(w) => w.write(buf),
            WriterInner::Zstd(w) => w.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match &mut self.inner {
            WriterInner::Plain(w) => w.flush(),
            WriterInner::Gzip(w) => w.flush(),
            WriterInner::Bzip2(w) => w.flush(),
            WriterInner::Zstd(w) => w.flush(),
        }
    }
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

    /// `StreamWriter` output must decompress (via `read_bytes`) to the bytes written, for every
    /// extension — i.e. content-equivalent to `write_bytes`.
    fn stream_writer_round_trip(ext: &str) {
        let dir = std::env::temp_dir();
        let p = dir.join(format!("ngless_sw_{}_{}.{ext}", std::process::id(), ext));
        let ps = p.to_string_lossy().to_string();
        let payload = b"@r1\nACGTACGTACGT\n+\nIIIIIIIIIIII\n@r2\nTTTT\n+\nJJJJ\n";
        let mut w = StreamWriter::create(&ps).unwrap();
        // Mix of raw writes and write_line to exercise both paths.
        w.write_all(&payload[..20]).unwrap();
        w.write_all(&payload[20..]).unwrap();
        w.finish().unwrap();
        assert_eq!(read_bytes(&ps).unwrap(), payload);
        let _ = std::fs::remove_file(&p);
    }

    #[test]
    fn stream_writer_round_trips_all() {
        stream_writer_round_trip("fq");
        stream_writer_round_trip("fq.gz");
        stream_writer_round_trip("fq.bz2");
        stream_writer_round_trip("fq.zst");
    }

    /// Concatenating several decompressed inputs through one `StreamWriter` (via `io::copy`)
    /// yields the concatenation — what the multi-file `write` path relies on.
    #[test]
    fn stream_writer_concat_via_copy() {
        let dir = std::env::temp_dir();
        let pid = std::process::id();
        let a = dir.join(format!("ngless_swc_{pid}_a.fq.gz"));
        let b = dir.join(format!("ngless_swc_{pid}_b.fq.bz2"));
        let out = dir.join(format!("ngless_swc_{pid}_out.fq.gz"));
        let (as_, bs, os) = (
            a.to_string_lossy().to_string(),
            b.to_string_lossy().to_string(),
            out.to_string_lossy().to_string(),
        );
        write_bytes(&as_, b"@a\nAAAA\n+\nIIII\n").unwrap();
        write_bytes(&bs, b"@b\nCCCC\n+\nJJJJ\n").unwrap();
        let mut w = StreamWriter::create(&os).unwrap();
        std::io::copy(&mut open_read(&as_).unwrap(), &mut w).unwrap();
        std::io::copy(&mut open_read(&bs).unwrap(), &mut w).unwrap();
        w.finish().unwrap();
        assert_eq!(
            read_to_string(&os).unwrap(),
            "@a\nAAAA\n+\nIIII\n@b\nCCCC\n+\nJJJJ\n"
        );
        for p in [&a, &b, &out] {
            let _ = std::fs::remove_file(p);
        }
    }

    /// `write_bytes_atomic` is content-equivalent to `write_bytes` (compresses per the final
    /// extension and round-trips), and leaves no temp file behind.
    fn atomic_round_trip(ext: &str) {
        let dir = std::env::temp_dir();
        let p = dir.join(format!("ngless_atomic_{}.{ext}", std::process::id()));
        let ps = p.to_string_lossy().to_string();
        let payload = "tag\tcount\nA\t3\nB\t5\n";
        write_bytes_atomic(&ps, payload.as_bytes()).unwrap();
        assert_eq!(read_to_string(&ps).unwrap(), payload);
        // No leftover `<path>.tmp.*` sibling.
        let stray: Vec<_> = std::fs::read_dir(&dir)
            .unwrap()
            .filter_map(|e| e.ok())
            .map(|e| e.file_name().to_string_lossy().to_string())
            .filter(|n| n.starts_with(&format!("ngless_atomic_{}.{ext}.tmp.", std::process::id())))
            .collect();
        assert!(stray.is_empty(), "atomic write left a temp file: {stray:?}");
        let _ = std::fs::remove_file(&p);
    }

    #[test]
    fn write_bytes_atomic_round_trips() {
        atomic_round_trip("tsv");
        atomic_round_trip("tsv.gz");
        atomic_round_trip("tsv.bz2");
        atomic_round_trip("tsv.zst");
    }
}
