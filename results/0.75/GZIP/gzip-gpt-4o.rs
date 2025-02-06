// Cargo.toml dependencies:
// [dependencies]
// nom = "7"
// clap = { version = "4.1", features = ["derive"] }
// thiserror = "1.0"

use clap::Parser;
use nom::bytes::complete::{take, take_while};
use nom::combinator::map_opt;
use nom::number::complete::{le_u16, le_u32, le_u8};
use nom::sequence::tuple;
use nom::IResult;
use std::fs::File;
use std::io::{self, Read};
use thiserror::Error;

#[derive(Debug, Parser)]
struct Args {
    /// GZIP file to parse
    #[clap(short, long)]
    input: String,
}

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    cm: u8,
    flg: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
    extra: Option<Vec<u8>>,
    fname: Option<String>,
    fcomment: Option<String>,
    fhcrc: Option<u16>,
}

#[derive(Debug, Error)]
enum GzipError {
    #[error("Nom parsing error: {0}")]
    NomError(String),
    #[error("IO error: {0}")]
    IoError(#[from] io::Error),
    #[error("UTF-8 error: {0}")]
    Utf8Error(#[from] std::str::Utf8Error),
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, cm, flg, mtime, xfl, os)) =
        tuple((le_u8, le_u8, le_u8, le_u8, le_u32, le_u8, le_u8))(input)?;
    
    let (input, extra) = if (flg & 0x04) != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra) = take(xlen)(input)?;
        (input, Some(extra.to_vec()))
    } else {
        (input, None)
    };

    let (input, fname) = if (flg & 0x08) != 0 {
        let (input, fname) = take_until_null(input)?;
        (input, Some(fname))
    } else {
        (input, None)
    };

    let (input, fcomment) = if (flg & 0x10) != 0 {
        let (input, fcomment) = take_until_null(input)?;
        (input, Some(fcomment))
    } else {
        (input, None)
    };

    let (input, fhcrc) = if (flg & 0x02) != 0 {
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
            cm,
            flg,
            mtime,
            xfl,
            os,
            extra,
            fname,
            fcomment,
            fhcrc,
        },
    ))
}

fn take_until_null(input: &[u8]) -> IResult<&[u8], String> {
    map_opt(take_while_including_null, |bytes: &[u8]| {
        std::str::from_utf8(&bytes[..bytes.len()]).map(String::from).ok()
    })(input)
}

fn take_while_including_null(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let (input, bytes) = take_while(|b| b != 0)(input)?;
    let (input, _) = take(1usize)(input)?;
    Ok((input, bytes))
}

fn main() -> Result<(), GzipError> {
    let args = Args::parse();

    let mut file = File::open(args.input)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => {
            println!("{:#?}", header);
            Ok(())
        }
        Err(e) => Err(GzipError::NomError(format!("{:?}", e))),
    }
}