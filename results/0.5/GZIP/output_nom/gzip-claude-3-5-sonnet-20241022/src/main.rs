use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, u8},
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
    mtime: u32,
    extra_flags: u8,
    os: u8,
    extra_field: Option<Vec<u8>>,
    filename: Option<String>,
    comment: Option<String>,
    header_crc: Option<u16>,
}

#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    compressed_data: Vec<u8>,
    crc32: u32,
    isize: u32,
}

fn parse_magic_numbers(input: &[u8]) -> IResult<&[u8], (u8, u8)> {
    let (input, id1) = tag(&[0x1f])(input)?;
    let (input, id2) = tag(&[0x8b])(input)?;
    Ok((input, (id1[0], id2[0])))
}

fn parse_null_terminated_string(input: &[u8]) -> IResult<&[u8], String> {
    let mut end = 0;
    while end < input.len() && input[end] != 0 {
        end += 1;
    }
    if end >= input.len() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }
    let (remaining, string_bytes) = take(end + 1)(input)?;
    Ok((
        remaining,
        String::from_utf8_lossy(&string_bytes[..end]).to_string(),
    ))
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2)) = parse_magic_numbers(input)?;
    let (input, compression_method) = u8(input)?;
    let (input, flags) = u8(input)?;
    let (input, mtime) = le_u32(input)?;
    let (input, extra_flags) = u8(input)?;
    let (input, os) = u8(input)?;

    let mut current_input = input;
    let mut extra_field = None;
    let mut filename = None;
    let mut comment = None;
    let mut header_crc = None;

    // Parse optional fields based on flags
    if flags & 0x04 != 0 {
        // FEXTRA
        let (remaining, xlen) = le_u16(current_input)?;
        let (remaining, extra_data) = take(xlen)(remaining)?;
        extra_field = Some(extra_data.to_vec());
        current_input = remaining;
    }

    if flags & 0x08 != 0 {
        // FNAME
        let (remaining, fname) = parse_null_terminated_string(current_input)?;
        filename = Some(fname);
        current_input = remaining;
    }

    if flags & 0x10 != 0 {
        // FCOMMENT
        let (remaining, fcomment) = parse_null_terminated_string(current_input)?;
        comment = Some(fcomment);
        current_input = remaining;
    }

    if flags & 0x02 != 0 {
        // FHCRC
        let (remaining, crc16) = le_u16(current_input)?;
        header_crc = Some(crc16);
        current_input = remaining;
    }

    Ok((
        current_input,
        GzipHeader {
            id1,
            id2,
            compression_method,
            flags,
            mtime,
            extra_flags,
            os,
            extra_field,
            filename,
            comment,
            header_crc,
        },
    ))
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    
    // The compressed data extends until the last 8 bytes
    let compressed_data_len = input.len() - 8;
    let (input, compressed_data) = take(compressed_data_len)(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, isize) = le_u32(input)?;

    Ok((
        input,
        GzipFile {
            header,
            compressed_data: compressed_data.to_vec(),
            crc32,
            isize,
        },
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip-file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_file(&buffer) {
        Ok((remaining, gzip_file)) => {
            if remaining.is_empty() {
                println!("Successfully parsed GZIP file:");
                println!("Header: {:#?}", gzip_file.header);
                println!("Compressed data length: {} bytes", gzip_file.compressed_data.len());
                println!("CRC32: {:08x}", gzip_file.crc32);
                println!("Original size: {} bytes", gzip_file.isize);
            } else {
                println!("Warning: {} unparsed bytes remaining", remaining.len());
            }
        }
        Err(e) => {
            eprintln!("Failed to parse GZIP file: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}