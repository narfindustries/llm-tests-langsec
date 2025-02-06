use nom::{
    bytes::complete::{take, take_until},
    combinator::{map, map_res, opt},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::{
    fs::File,
    io::{self, Read},
    path::PathBuf,
};
use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    #[clap(parse(from_os_str))]
    input_file: PathBuf,
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
    name: Option<String>,
    comment: Option<String>,
    hcrc: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    map(
        tuple((
            le_u8,
            le_u8,
            le_u8,
            le_u8,
            le_u32,
            le_u8,
            le_u8,
            parse_optional_fields,
        )),
        |(id1, id2, cm, flg, mtime, xfl, os, (extra, name, comment, hcrc))| GzipHeader {
            id1,
            id2,
            cm,
            flg,
            mtime,
            xfl,
            os,
            extra,
            name,
            comment,
            hcrc,
        },
    )(input)
}

fn parse_optional_fields(input: &[u8], flg: u8) -> IResult<&[u8], (Option<Vec<u8>>, Option<String>, Option<String>, Option<u16>)> {
    let (input, extra) = if flg & 0x04 != 0 {
        let (i, len) = le_u16(input)?;
        let (i, data) = take(len)(i)?;
        (i, Some(data.to_vec()))
    } else {
        (input, None)
    };

    let (input, name) = if flg & 0x08 != 0 {
        map_res(preceded(take_until("\0"), take(1)), |bytes: &[u8]| {
            std::str::from_utf8(&bytes[..bytes.len() - 1]).map(|s| s.to_string())
        })(input)?
    } else {
        (input, None)
    };

    let (input, comment) = if flg & 0x10 != 0 {
        map_res(preceded(take_until("\0"), take(1)), |bytes: &[u8]| {
            std::str::from_utf8(&bytes[..bytes.len() - 1]).map(|s| s.to_string())
        })(input)?
    } else {
        (input, None)
    };

    let (input, hcrc) = if flg & 0x02 != 0 {
        map(opt(le_u16), |v| v)(input)?
    } else {
        (input, None)
    };

    Ok((input, (extra, name, comment, hcrc)))
}

fn main() -> io::Result<()> {
    let args = Cli::parse();
    let mut file = File::open(args.input_file)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => {
            println!("{:#?}", header);
        }
        Err(e) => eprintln!("Failed to parse GZIP header: {:?}", e),
    }

    Ok(())
}