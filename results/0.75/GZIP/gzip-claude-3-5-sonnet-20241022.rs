use nom::{
    bits::{bits, complete::take},
    branch::alt,
    bytes::complete::{tag, take as take_bytes},
    combinator::{map, verify},
    error::Error,
    multi::many0,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::{env, fs, io::Read, path::Path};

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

fn parse_flags(input: &[u8]) -> IResult<&[u8], GzipFlags> {
    bits(|input| {
        let (input, (
            ftext,
            fhcrc,
            fextra,
            fname,
            fcomment,
            reserved,
        )): (_, (u8, u8, u8, u8, u8, u8)) = tuple((
            take(1usize),
            take(1usize),
            take(1usize),
            take(1usize),
            take(1usize),
            take(3usize),
        ))(input)?;

        Ok((input, GzipFlags {
            ftext: ftext == 1,
            fhcrc: fhcrc == 1,
            fextra: fextra == 1,
            fname: fname == 1,
            fcomment: fcomment == 1,
            reserved,
        }))
    })(input)
}

fn parse_null_terminated_string(input: &[u8]) -> IResult<&[u8], String> {
    let mut bytes = Vec::new();
    let mut remaining = input;

    while !remaining.is_empty() && remaining[0] != 0 {
        bytes.push(remaining[0]);
        remaining = &remaining[1..];
    }

    if remaining.is_empty() {
        return Err(nom::Err::Error(Error::new(input, nom::error::ErrorKind::Tag)));
    }

    Ok((&remaining[1..], String::from_utf8_lossy(&bytes).into_owned()))
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2)) = verify(
        tuple((take_bytes(1usize), take_bytes(1usize))),
        |(id1, id2): (&[u8], &[u8])| id1[0] == 0x1f && id2[0] == 0x8b,
    )(input)?;

    let (input, compression_method) = verify(
        map(take_bytes(1usize), |b: &[u8]| b[0]),
        |&method| method == 8,
    )(input)?;

    let (input, flags) = parse_flags(input)?;
    let (input, mtime) = le_u32(input)?;
    let (input, extra_flags) = map(take_bytes(1usize), |b: &[u8]| b[0])(input)?;
    let (input, os) = map(take_bytes(1usize), |b: &[u8]| b[0])(input)?;

    let (input, extra_field) = if flags.fextra {
        let (input, xlen) = le_u16(input)?;
        let (input, extra) = take_bytes(xlen as usize)(input)?;
        (input, Some(extra.to_vec()))
    } else {
        (input, None)
    };

    let (input, filename) = if flags.fname {
        let (input, name) = parse_null_terminated_string(input)?;
        (input, Some(name))
    } else {
        (input, None)
    };

    let (input, comment) = if flags.fcomment {
        let (input, comment) = parse_null_terminated_string(input)?;
        (input, Some(comment))
    } else {
        (input, None)
    };

    let (input, crc16) = if flags.fhcrc {
        let (input, crc) = le_u16(input)?;
        (input, Some(crc))
    } else {
        (input, None)
    };

    Ok((input, GzipHeader {
        id1: id1[0],
        id2: id2[0],
        compression_method,
        flags,
        mtime,
        extra_flags,
        os,
        extra_field,
        filename,
        comment,
        crc16,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip-file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = fs::File::open(path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Failed to parse GZIP header: {:?}", e),
    }
}