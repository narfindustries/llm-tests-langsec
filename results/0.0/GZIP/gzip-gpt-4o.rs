use std::fs::File;
use std::io::Read;
use std::path::Path;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    number::complete::{le_u16, le_u32, u8},
    sequence::tuple,
    IResult,
};

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
    hcrc: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, cm, flg, mtime, xfl, os)) = tuple((u8, u8, u8, u8, le_u32, u8, u8))(input)?;

    let (input, extra) = if flg & 0x04 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra) = take(xlen)(input)?;
        (input, Some(extra.to_vec()))
    } else {
        (input, None)
    };

    let (input, filename) = if flg & 0x08 != 0 {
        let (input, filename) = take_until_null(input)?;
        (input, Some(filename))
    } else {
        (input, None)
    };

    let (input, comment) = if flg & 0x10 != 0 {
        let (input, comment) = take_until_null(input)?;
        (input, Some(comment))
    } else {
        (input, None)
    };

    let (input, hcrc) = if flg & 0x02 != 0 {
        let (input, hcrc) = le_u16(input)?;
        (input, Some(hcrc))
    } else {
        (input, None)
    };

    Ok((input, GzipHeader { id1, id2, cm, flg, mtime, xfl, os, extra, filename, comment, hcrc }))
}

fn take_until_null(input: &[u8]) -> IResult<&[u8], String> {
    let pos = input.iter().position(|&b| b == 0).unwrap_or(input.len());
    let (left, right) = input.split_at(pos);
    let (right, _) = take(1usize)(right)?;
    Ok((right, String::from_utf8_lossy(left).into_owned()))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => eprintln!("Failed to parse GZIP header: {:?}", e),
    }
}