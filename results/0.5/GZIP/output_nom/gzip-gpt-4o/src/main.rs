extern crate nom;

use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt, map_res},
    multi::count,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

// GZIP Header
#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: u8,
    mtime: u32,
    extra_flags: u8,
    os: u8,
    extra_field: Option<Vec<u8>>,
    filename: Option<String>,
    comment: Option<String>,
    crc16: Option<u16>,
}

// GZIP Footer
#[derive(Debug)]
struct GzipFooter {
    crc32: u32,
    isize: u32,
}

// GZIP File
#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    compressed_data: Vec<u8>,
    footer: GzipFooter,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, compression_method, flags, mtime, extra_flags, os)) = tuple((
        le_u8, le_u8, le_u8, le_u8, le_u32, le_u8, le_u8,
    ))(input)?;

    let (input, extra_field) = if flags & 0b00000100 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra_field) = take(xlen)(input)?;
        (input, Some(extra_field.to_vec()))
    } else {
        (input, None)
    };

    let (input, filename) = if flags & 0b00001000 != 0 {
        let (input, filename) = take_while_not_null(input)?;
        (input, Some(filename))
    } else {
        (input, None)
    };

    let (input, comment) = if flags & 0b00010000 != 0 {
        let (input, comment) = take_while_not_null(input)?;
        (input, Some(comment))
    } else {
        (input, None)
    };

    let (input, crc16) = if flags & 0b00000010 != 0 {
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
            extra_flags,
            os,
            extra_field,
            filename,
            comment,
            crc16,
        },
    ))
}

fn take_while_not_null(input: &[u8]) -> IResult<&[u8], String> {
    map_res(
        map_opt(take_until_null, |bytes: &[u8]| {
            std::str::from_utf8(bytes).ok()
        }),
        |s: &str| s.to_string(),
    )(input)
}

fn take_until_null(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let position = input.iter().position(|&b| b == 0).unwrap_or(input.len());
    take(position)(input)
}

fn parse_gzip_footer(input: &[u8]) -> IResult<&[u8], GzipFooter> {
    let (input, (crc32, isize)) = tuple((le_u32, le_u32))(input)?;
    Ok((input, GzipFooter { crc32, isize }))
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    let (input, compressed_data) = take(input.len() - 8)(input)?;
    let (input, footer) = parse_gzip_footer(input)?;

    Ok((
        input,
        GzipFile {
            header,
            compressed_data: compressed_data.to_vec(),
            footer,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
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