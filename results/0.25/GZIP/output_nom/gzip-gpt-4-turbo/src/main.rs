use nom::{
    bytes::complete::{tag, take, take_until},
    combinator::{map, map_opt, opt},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::path::PathBuf;
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
    filename: Option<String>,
    comment: Option<String>,
    crc16: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, cm, flg, mtime, xfl, os)) = tuple((
        tag([0x1f, 0x8b]), // ID1 and ID2
        tag([0x08]),        // CM
        le_u32,             // MTIME
        le_u8,              // XFL
        le_u8,              // OS
    ))(input)?;

    let (input, extra) = if flg & 0x04 != 0 {
        let (input, len) = le_u16(input)?;
        let (input, data) = take(len)(input)?;
        (input, Some(data.to_vec()))
    } else {
        (input, None)
    };

    let (input, filename) = if flg & 0x08 != 0 {
        map_opt(preceded(tag([0]), take_until("\0")), |s: &[u8]| {
            String::from_utf8(s.to_vec()).ok()
        })(input)?
    } else {
        (input, None)
    };

    let (input, comment) = if flg & 0x10 != 0 {
        map_opt(preceded(tag([0]), take_until("\0")), |s: &[u8]| {
            String::from_utf8(s.to_vec()).ok()
        })(input)?
    } else {
        (input, None)
    };

    let (input, crc16) = if flg & 0x02 != 0 {
        map(le_u16, Some)(input)?
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
            filename,
            comment,
            crc16,
        },
    ))
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
        Err(e) => {
            eprintln!("Failed to parse GZIP header: {:?}", e);
        }
    }

    Ok(())
}