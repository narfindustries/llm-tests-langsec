use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt},
    error::ErrorKind,
    multi::{count, many0},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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

#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    compressed_data: Vec<u8>,
    crc32: u32,
    original_size: u32,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2)) = tuple((le_u8, le_u8))(input)?;
    let (input, compression_method) = le_u8(input)?;
    let (input, flags) = le_u8(input)?;
    let (input, modification_time) = le_u32(input)?;
    let (input, extra_flags) = le_u8(input)?;
    let (input, os) = le_u8(input)?;

    let (input, extra_field) = if flags & 0x04 != 0 {
        let (input, extra_length) = le_u16(input)?;
        let (input, extra_data) = take(extra_length as usize)(input)?;
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

    Ok((input, GzipHeader {
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
    }))
}

fn take_until_zero(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let zero_pos = input.iter().position(|&x| x == 0).ok_or(nom::Err::Error(ErrorKind::TakeUntil))?;
    Ok((&input[zero_pos + 1..], &input[..zero_pos]))
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    let (input, compressed_data) = take_compressed_data(input, header.flags)?;
    let (input, crc32) = le_u32(input)?;
    let (input, original_size) = le_u32(input)?;

    Ok((input, GzipFile {
        header,
        compressed_data: compressed_data.to_vec(),
        crc32,
        original_size,
    }))
}

fn take_compressed_data(input: &[u8], flags: u8) -> IResult<&[u8], &[u8]> {
    let compressed_data_end = input.len() - 8;
    Ok((&input[compressed_data_end..], &input[..compressed_data_end]))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_file(&buffer) {
        Ok((_, gzip_file)) => {
            println!("Parsed GZIP file: {:?}", gzip_file);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing GZIP file: {:?}", e);
            std::process::exit(1);
        }
    }
}