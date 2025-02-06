use nom::{
    bytes::complete::{tag, take, take_while},
    combinator::{map, map_res, opt},
    number::complete::{le_u16, le_u32, u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;
use std::str;

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
    header_crc16: Option<u16>,
}

#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    compressed_data: Vec<u8>,
    crc32: u32,
    isize: u32,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, compression_method, flags, mtime, xfl, os)) =
        tuple((u8, u8, u8, u8, le_u32, u8, u8))(input)?;

    let (input, extra) = if flags & 0x04 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra) = take(xlen)(input)?;
        (input, Some(extra.to_vec()))
    } else {
        (input, None)
    };

    let (input, filename) = if flags & 0x08 != 0 {
        let (input, filename_bytes) = take_while(|c| c != 0)(input)?;
        let (input, _) = tag([0])(input)?;
        (input, Some(str::from_utf8(filename_bytes).unwrap().to_string()))
    } else {
        (input, None)
    };

    let (input, comment) = if flags & 0x10 != 0 {
        let (input, comment_bytes) = take_while(|c| c != 0)(input)?;
        let (input, _) = tag([0])(input)?;
        (input, Some(str::from_utf8(comment_bytes).unwrap().to_string()))
    } else {
        (input, None)
    };

    let (input, header_crc16) = if flags & 0x02 != 0 {
        let (input, crc16) = le_u16(input)?;
        (input, Some(crc16))
    } else {
        (input, None)
    };

    Ok((
        input,
        GzipHeader {
            id1,
            id2,
            compression_method,
            flags,
            mtime,
            xfl,
            os,
            extra,
            filename,
            comment,
            header_crc16,
        },
    ))
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    let (input, compressed_data) = take(input.len() - 8)(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, isize) = le_u32(input)?;

    Ok((
        input,
        GzipFile {
            header,
            compressed_data: compressed_data.to_vec(),
            crc32,
            isize,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_file(&buffer) {
        Ok((_, gzip_file)) => {
            println!("{:#?}", gzip_file);
        }
        Err(e) => {
            eprintln!("Failed to parse GZIP file: {:?}", e);
        }
    }

    Ok(())
}