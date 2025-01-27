use std::fs::File;
use std::io::Read;
use std::path::Path;
use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::many_till,
    number::complete::{le_u16, le_u32, le_u64},
    IResult,
};

#[derive(Debug)]
struct LocalFileHeader {
    version: u16,
    flags: u16,
    compression: u16,
    mod_time: u16,
    mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
    file_data: Vec<u8>,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, _) = tag(&[0x50, 0x4B, 0x03, 0x04])(input)?; // Signature for Local File Header
    let (input, version) = le_u16(input)?;
    let (input, flags) = le_u16(input)?;
    let (input, compression) = le_u16(input)?;
    let (input, mod_time) = le_u16(input)?;
    let (input, mod_date) = le_u16(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, compressed_size) = le_u32(input)?;
    let (input, uncompressed_size) = le_u32(input)?;
    let (input, file_name_length) = le_u16(input)?;
    let (input, extra_field_length) = le_u16(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_data) = take(compressed_size)(input)?;

    Ok((input, LocalFileHeader {
        version,
        flags,
        compression,
        mod_time,
        mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        file_name: file_name.to_vec(),
        extra_field: extra_field.to_vec(),
        file_data: file_data.to_vec(),
    }))
}

fn parse_zip(input: &[u8]) -> IResult<&[u8], Vec<LocalFileHeader>> {
    many_till(parse_local_file_header, eof)(input).map(|(i, (files, _))| (i, files))
}

fn eof(input: &[u8]) -> IResult<&[u8], ()> {
    if input.is_empty() {
        Ok((input, ()))
    } else {
        Err(nom::Err::Error((input, nom::error::ErrorKind::Eof)))
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Failed to open file: {}", e);
            std::process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Failed to read file: {}", e);
            std::process::exit(1);
        }
    };

    match parse_zip(&buffer) {
        Ok((_, headers)) => {
            for header in headers {
                println!("{:?}", header);
            }
        }
        Err(e) => {
            eprintln!("Failed to parse ZIP file: {:?}", e);
        }
    }
}