use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

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
    original_filename: Option<String>,
    comment: Option<String>,
    crc16: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, compression_method, flags, mtime, extra_flags, os)) =
        tuple((le_u8, le_u8, le_u8, le_u8, le_u32, le_u8, le_u8))(input)?;

    if id1 != 0x1f || id2 != 0x8b {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)));
    }

    let (mut input, extra) = if flags & 0x04 != 0 {
        let (i, extra_len) = le_u16(input)?;
        let (i, extra_data) = take(extra_len)(i)?;
        (i, Some(extra_data.to_vec()))
    } else {
        (input, None)
    };

    let (input, original_filename) = if flags & 0x08 != 0 {
        let (i, filename) = nom::character::complete::c_string(input)?;
        (i, Some(filename.to_owned()))
    } else {
        (input, None)
    };

    let (input, comment) = if flags & 0x10 != 0 {
        let (i, comment_str) = nom::character::complete::c_string(input)?;
        (i, Some(comment_str.to_owned()))
    } else {
        (input, None)
    };

    let (input, crc16) = if flags & 0x02 != 0 {
        let (i, crc) = le_u16(input)?;
        (i, Some(crc))
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
            original_filename,
            comment,
            crc16,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
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