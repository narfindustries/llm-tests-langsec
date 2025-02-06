use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map_res, opt},
    error::{context, VerboseError, VerboseErrorKind},
    number::complete::{be_u16},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs;
use std::path::Path;

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

fn gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader, VerboseError<&[u8]>> {
    let (rest, (id1, id2, cm, flg, mtime, xfl, os)) = tuple((
        take(1usize),
        take(1usize),
        take(1usize),
        take(1usize),
        take(4usize),
        take(1usize),
        take(1usize),
    ))(input)?;

    let (rest, extra) = opt(preceded(
        context("extra_field", |i| {
            if (flg & 0b0000_0100) != 0 {
                Ok((i, ()))
            } else {
                Err(nom::Err::Error(VerboseError {
                    errors: vec![(i, VerboseErrorKind::Nom(nom::error::ErrorKind::Tag))],
                }))
            }
        }),
        context("extra_data", |i: &[u8]| {
            let (i, len) = be_u16(i)?;
            let (i, data) = take(len as usize)(i)?;
            Ok((i, data.to_vec()))
        }),
    ))(rest)?;

    let (rest, fname) = opt(preceded(
        context("fname_field", |i| {
            if (flg & 0b0000_1000) != 0 {
                Ok((i, ()))
            } else {
                Err(nom::Err::Error(VerboseError {
                    errors: vec![(i, VerboseErrorKind::Nom(nom::error::ErrorKind::Tag))],
                }))
            }
        }),
        context("fname", map_res(take_while_m_n(1, 255, |b| b != 0), |bytes: &[u8]| {
            String::from_utf8(bytes.to_vec())
        })),
    ))(rest)?;

    let (rest, fcomment) = opt(preceded(
        context("fcomment_field", |i| {
            if (flg & 0b0001_0000) != 0 {
                Ok((i, ()))
            } else {
                Err(nom::Err::Error(VerboseError {
                    errors: vec![(i, VerboseErrorKind::Nom(nom::error::ErrorKind::Tag))],
                }))
            }
        }),
        context("fcomment", map_res(take_while_m_n(1, 255, |b| b != 0), |bytes: &[u8]| {
            String::from_utf8(bytes.to_vec())
        })),
    ))(rest)?;

    let (rest, fhcrc) = opt(preceded(
        context("fhcrc_field", |i| {
            if (flg & 0b0000_0010) != 0 {
                Ok((i, ()))
            } else {
                Err(nom::Err::Error(VerboseError {
                    errors: vec![(i, VerboseErrorKind::Nom(nom::error::ErrorKind::Tag))],
                }))
            }
        }),
        be_u16,
    ))(rest)?;

    Ok((
        rest,
        GzipHeader {
            id1: id1[0],
            id2: id2[0],
            cm: cm[0],
            flg: flg[0],
            mtime: u32::from_le_bytes(mtime.try_into().unwrap()),
            xfl: xfl[0],
            os: os[0],
            extra,
            fname,
            fcomment,
            fhcrc,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip_file>", args[0]);
        std::process::exit(1);
    }

    let file_path = Path::new(&args[1]);
    let file_contents = match fs::read(file_path) {
        Ok(contents) => contents,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            std::process::exit(1);
        }
    };

    match gzip_header(&file_contents) {
        Ok((rest, header)) => {
            println!("Gzip Header: {:?}", header);
            println!("Remaining data: {} bytes", rest.len());
        }
        Err(e) => {
            println!("Error parsing GZIP header: {:?}", e);
        }
    }
}
