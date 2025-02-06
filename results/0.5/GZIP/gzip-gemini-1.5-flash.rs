use nom::{
    bytes::complete::{tag, take, take_while_m_n},
    combinator::{map, map_res, opt, verify},
    error::ParseError,
    number::complete::{be_u16, be_u32},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{Read, Result};
use std::path::Path;
use std::string::FromUtf8Error;

#[derive(Debug, PartialEq)]
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
    comment: Option<String>,
    header_crc: Option<u16>,
}

fn gzip_header<'a, E: ParseError<&'a [u8]> + nom::error::FromExternalError<&'a [u8], FromUtf8Error>>(
    input: &'a [u8],
) -> IResult<&'a [u8], GzipHeader, E> {
    let (input, (id1, id2, cm, flg, mtime, xfl, os)) = tuple((
        take(1usize),
        take(1usize),
        take(1usize),
        take(1usize),
        be_u32,
        take(1usize),
        take(1usize),
    ))(input)?;

    let (input, extra_len) = opt(preceded(
        verify(take(2usize), |len: &[u8]| len[0] == 0 && len[1] == 0),
        be_u16,
    ))(input)?;

    let (input, extra) = match extra_len {
        Some(len) => {
            let (i, e) = take(len as usize)(input)?;
            (i, Some(e.to_vec()))
        }
        None => (input, None),
    };


    let (input, fname) = opt(preceded(
        tag(b"\0"),
        map_res(take_while_m_n(1, 255, |b| b != 0), |bytes: &[u8]| {
            String::from_utf8(bytes.to_vec())
        }),
    ))(input)?;

    let (input, comment) = opt(preceded(
        tag(b"\0"),
        map_res(take_while_m_n(1, 255, |b| b != 0), |bytes: &[u8]| {
            String::from_utf8(bytes.to_vec())
        }),
    ))(input)?;

    let (input, header_crc) = opt(be_u16)(input)?;

    Ok((
        input,
        GzipHeader {
            id1: id1[0],
            id2: id2[0],
            cm: cm[0],
            flg: flg[0],
            mtime,
            xfl: xfl[0],
            os: os[0],
            extra,
            fname,
            comment,
            header_crc,
        },
    ))
}

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match gzip_header::<nom::error::VerboseError<&[u8]>>(&buffer) {
        Ok((remaining, header)) => {
            println!("Parsed GZIP header: {:?}", header);
            println!("Remaining data: {:?}", remaining);
        }
        Err(e) => {
            println!("Error parsing GZIP header: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}
