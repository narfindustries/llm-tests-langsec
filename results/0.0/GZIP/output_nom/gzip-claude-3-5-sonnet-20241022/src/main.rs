use nom::{
    bits::complete::{tag, take},
    branch::alt,
    bytes::complete::{take as take_bytes},
    combinator::{map, verify},
    error::Error,
    multi::many0,
    sequence::tuple,
    IResult,
};
use std::{env, fs, io::Write};

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: GzipFlags,
    mtime: u32,
    extra_flags: u8,
    os: u8,
    extra_field: Option<Vec<u8>>,
    filename: Option<String>,
    comment: Option<String>,
    crc16: Option<u16>,
}

#[derive(Debug)]
struct GzipFlags {
    ftext: bool,
    fhcrc: bool,
    fextra: bool,
    fname: bool,
    fcomment: bool,
    reserved: u8,
}

fn parse_flags(input: (&[u8], usize)) -> IResult<(&[u8], usize), GzipFlags> {
    let (input, flags) = tuple((
        take(1usize),
        take(1usize),
        take(1usize),
        take(1usize),
        take(1usize),
        take(3usize),
    ))(input)?;

    Ok((
        input,
        GzipFlags {
            ftext: flags.0 == 1,
            fhcrc: flags.1 == 1,
            fextra: flags.2 == 1,
            fname: flags.3 == 1,
            fcomment: flags.4 == 1,
            reserved: flags.5 as u8,
        },
    ))
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, id1) = verify(take_bytes(1u8), |b: &[u8]| b[0] == 0x1f)(input)?;
    let (input, id2) = verify(take_bytes(1u8), |b: &[u8]| b[0] == 0x8b)(input)?;
    let (input, compression_method) = verify(take_bytes(1u8), |b: &[u8]| b[0] == 8)(input)?;
    
    let (input, flags) = nom::bits::bits(parse_flags)(input)?;
    let (input, mtime) = take_bytes(4u8)(input)?;
    let (input, extra_flags) = take_bytes(1u8)(input)?;
    let (input, os) = take_bytes(1u8)(input)?;

    let mut remaining = input;
    let mut extra_field = None;
    let mut filename = None;
    let mut comment = None;
    let mut crc16 = None;

    if flags.fextra {
        let (input, xlen) = take_bytes(2u8)(remaining)?;
        let xlen = ((xlen[1] as u16) << 8) | xlen[0] as u16;
        let (input, extra) = take_bytes(xlen)(input)?;
        extra_field = Some(extra.to_vec());
        remaining = input;
    }

    if flags.fname {
        let mut name_bytes = Vec::new();
        let mut current_input = remaining;
        loop {
            let (input, byte) = take_bytes(1u8)(current_input)?;
            if byte[0] == 0 {
                remaining = input;
                break;
            }
            name_bytes.push(byte[0]);
            current_input = input;
        }
        filename = Some(String::from_utf8_lossy(&name_bytes).to_string());
    }

    if flags.fcomment {
        let mut comment_bytes = Vec::new();
        let mut current_input = remaining;
        loop {
            let (input, byte) = take_bytes(1u8)(current_input)?;
            if byte[0] == 0 {
                remaining = input;
                break;
            }
            comment_bytes.push(byte[0]);
            current_input = input;
        }
        comment = Some(String::from_utf8_lossy(&comment_bytes).to_string());
    }

    if flags.fhcrc {
        let (input, crc) = take_bytes(2u8)(remaining)?;
        crc16 = Some(((crc[1] as u16) << 8) | crc[0] as u16);
        remaining = input;
    }

    Ok((
        remaining,
        GzipHeader {
            id1: id1[0],
            id2: id2[0],
            compression_method: compression_method[0],
            flags,
            mtime: ((mtime[3] as u32) << 24)
                | ((mtime[2] as u32) << 16)
                | ((mtime[1] as u32) << 8)
                | mtime[0] as u32,
            extra_flags: extra_flags[0],
            os: os[0],
            extra_field,
            filename,
            comment,
            crc16,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip-file>", args[0]);
        std::process::exit(1);
    }

    let data = fs::read(&args[1]).expect("Failed to read file");
    match parse_gzip_header(&data) {
        Ok((remaining, header)) => {
            println!("GZIP Header: {:#?}", header);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse GZIP header: {:?}", e);
            std::process::exit(1);
        }
    }
}