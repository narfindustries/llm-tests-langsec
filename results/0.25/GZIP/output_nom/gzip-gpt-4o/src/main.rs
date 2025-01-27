// Cargo.toml dependencies:
// [dependencies]
// nom = "7.1"
// clap = { version = "4.0", features = ["derive"] }

use clap::Parser;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt, map_res, opt},
    error::Error,
    multi::many0,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::path::PathBuf;

#[derive(Parser)]
struct Cli {
    /// Input binary file
    input: PathBuf,
}

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: u8,
    mtime: u32,
    extra_flags: u8,
    os: u8,
    extra: Option<Vec<u8>>,
    filename: Option<String>,
    comment: Option<String>,
    crc16: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2)) = tuple((le_u8, le_u8))(input)?;
    if id1 != 0x1f || id2 != 0x8b {
        return Err(nom::Err::Error(Error::new(input, nom::error::ErrorKind::Tag)));
    }

    let (input, compression_method) = le_u8(input)?;
    let (input, flags) = le_u8(input)?;
    let (input, mtime) = le_u32(input)?;
    let (input, extra_flags) = le_u8(input)?;
    let (input, os) = le_u8(input)?;

    let (input, extra) = if flags & 0b00000100 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra) = take(xlen)(input)?;
        (input, Some(extra.to_vec()))
    } else {
        (input, None)
    };

    let (input, filename) = if flags & 0b00001000 != 0 {
        let (input, filename) = map_res(take_until_null, |bytes: &[u8]| {
            std::str::from_utf8(bytes).map(String::from)
        })(input)?;
        (input, Some(filename))
    } else {
        (input, None)
    };

    let (input, comment) = if flags & 0b00010000 != 0 {
        let (input, comment) = map_res(take_until_null, |bytes: &[u8]| {
            std::str::from_utf8(bytes).map(String::from)
        })(input)?;
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
            extra,
            filename,
            comment,
            crc16,
        },
    ))
}

fn take_until_null(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let (input, data) = take_while(|b| b != 0)(input)?;
    let (input, _) = tag(&[0])(input)?;
    Ok((input, data))
}

fn take_while<F>(cond: F) -> impl Fn(&[u8]) -> IResult<&[u8], &[u8]>
where
    F: Fn(u8) -> bool,
{
    move |input: &[u8]| {
        let pos = input.iter().position(|&b| !cond(b)).unwrap_or(input.len());
        Ok((&input[pos..], &input[..pos]))
    }
}

fn main() -> io::Result<()> {
    let args = Cli::parse();

    let mut file = File::open(args.input)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => eprintln!("Failed to parse GZIP header: {:?}", e),
    }

    Ok(())
}