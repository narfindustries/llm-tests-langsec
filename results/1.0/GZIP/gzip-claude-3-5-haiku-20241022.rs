use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::many0,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::process;

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: u8,
    modification_time: u32,
    extra_flags: u8,
    os: u8,
    extra_field: Option<Vec<u8>>,
    original_filename: Option<String>,
    file_comment: Option<String>,
    header_crc: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, compression_method, flags, modification_time, extra_flags, os)) = tuple((
        le_u8,
        le_u8,
        le_u8,
        le_u8,
        le_u32,
        le_u8,
        le_u8,
    ))(input)?;

    let (input, extra_field) = if flags & 0x04 != 0 {
        let (input, extra_len) = le_u16(input)?;
        let (input, extra_data) = take(extra_len as usize)(input)?;
        (input, Some(extra_data.to_vec()))
    } else {
        (input, None)
    };

    let (input, original_filename) = if flags & 0x08 != 0 {
        let (input, filename) = take_until_zero(input)?;
        (input, Some(String::from_utf8_lossy(filename).into_owned()))
    } else {
        (input, None)
    };

    let (input, file_comment) = if flags & 0x10 != 0 {
        let (input, comment) = take_until_zero(input)?;
        (input, Some(String::from_utf8_lossy(comment).into_owned()))
    } else {
        (input, None)
    };

    let (input, header_crc) = if flags & 0x02 != 0 {
        let (input, crc) = le_u16(input)?;
        (input, Some(crc))
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
            modification_time,
            extra_flags,
            os,
            extra_field,
            original_filename,
            file_comment,
            header_crc,
        },
    ))
}

fn take_until_zero(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let (input, result) = take_while(|c| c != 0)(input)?;
    let (input, _) = tag(&[0])(input)?;
    Ok((input, result))
}

fn take_while(condition: impl Fn(u8) -> bool) -> impl Fn(&[u8]) -> IResult<&[u8], &[u8]> {
    move |input: &[u8]| {
        let idx = input.iter().position(|&c| !condition(c)).unwrap_or(input.len());
        Ok((&input[idx..], &input[..idx]))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip_file>", args[0]);
        process::exit(1);
    }

    let mut file = match File::open(&args[1]) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Error opening file: {}", e);
            process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    if let Err(e) = file.read_to_end(&mut buffer) {
        eprintln!("Error reading file: {}", e);
        process::exit(1);
    }

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => {
            println!("Parsed GZIP Header: {:?}", header);
        }
        Err(e) => {
            eprintln!("Error parsing GZIP header: {}", e);
            process::exit(1);
        }
    }
}