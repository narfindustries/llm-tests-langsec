use nom::{
    bytes::complete::{tag, take, take_until},
    combinator::{cond, map, verify},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
    extra: Option<Vec<u8>>,
    filename: Option<String>,
    comment: Option<String>,
    crc16: Option<u16>,
}

#[derive(Debug)]
struct GzipFooter {
    crc32: u32,
    isize: u32,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, compression_method, flags, mtime, xfl, os)) = tuple((
        verify(le_u8, |v| *v == 0x1f),
        verify(le_u8, |v| *v == 0x8b),
        le_u8,
        le_u8,
        le_u32,
        le_u8,
        le_u8,
    ))(input)?;

    let (input, extra) = cond((flags & 0x04) != 0, length_data(le_u16))(input)?;
    let (input, filename) = cond((flags & 0x08) != 0, take_until("\0"))(input)?;
    let (input, comment) = cond((flags & 0x10) != 0, take_until("\0"))(input)?;
    let (input, crc16) = cond((flags & 0x02) != 0, le_u16)(input)?;

    let filename = filename.map(|bytes| String::from_utf8_lossy(bytes).to_string());
    let comment = comment.map(|bytes| String::from_utf8_lossy(bytes).to_string());

    Ok((
        input,
        GzipHeader {
            id1, id2, compression_method, flags, mtime, xfl, os,
            extra: extra.map(|e| e.to_vec()),
            filename,
            comment,
            crc16,
        },
    ))
}

fn parse_gzip_footer(input: &[u8]) -> IResult<&[u8], GzipFooter> {
    let (input, (crc32, isize)) = tuple((le_u32, le_u32))(input)?;
    Ok((input, GzipFooter { crc32, isize }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path).expect("Failed to open file");

    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let (_, header) = parse_gzip_header(&buffer).expect("Failed to parse GZIP header");
    println!("{:?}", header);

    // The body of GZIP contents is usually compressed data + footer, here we just ignore it.
    let footer_start = buffer.len() - 8; // Footer is 8 bytes from the end
    let (_, footer) = parse_gzip_footer(&buffer[footer_start..]).expect("Failed to parse GZIP footer");
    println!("{:?}", footer);
}