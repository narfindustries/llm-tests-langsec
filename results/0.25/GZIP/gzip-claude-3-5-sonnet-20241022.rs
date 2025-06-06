use nom::bytes::complete::{tag, take};
use nom::number::complete::{le_u16, le_u32, u8};
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct GzipHeader {
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

fn parse_magic_numbers(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0x1f, 0x8b])(input)?;
    Ok((input, ()))
}

fn parse_null_terminated_string(input: &[u8]) -> IResult<&[u8], String> {
    let mut i = 0;
    while i < input.len() && input[i] != 0 {
        i += 1;
    }
    let (remaining, string_bytes) = take(i)(input)?;
    let (remaining, _) = take(1u8)(remaining)?;
    let string = String::from_utf8_lossy(string_bytes).to_string();
    Ok((remaining, string))
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, _) = parse_magic_numbers(input)?;
    let (input, compression_method) = u8(input)?;
    let (input, flags) = u8(input)?;
    let (input, mtime) = le_u32(input)?;
    let (input, extra_flags) = u8(input)?;
    let (input, os) = u8(input)?;

    let mut extra_field = None;
    let mut filename = None;
    let mut comment = None;
    let mut header_crc = None;
    let mut current_input = input;

    // Parse optional fields based on flags
    if flags & 0x04 != 0 {
        // FEXTRA
        let (input, xlen) = le_u16(current_input)?;
        let (input, extra_data) = take(xlen)(input)?;
        extra_field = Some(extra_data.to_vec());
        current_input = input;
    }

    if flags & 0x08 != 0 {
        // FNAME
        let (input, fname) = parse_null_terminated_string(current_input)?;
        filename = Some(fname);
        current_input = input;
    }

    if flags & 0x10 != 0 {
        // FCOMMENT
        let (input, fcomment) = parse_null_terminated_string(current_input)?;
        comment = Some(fcomment);
        current_input = input;
    }

    if flags & 0x02 != 0 {
        // FHCRC
        let (input, crc16) = le_u16(current_input)?;
        header_crc = Some(crc16);
        current_input = input;
    }

    Ok((
        current_input,
        GzipHeader {
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
    
    // Find the position of the last 8 bytes (CRC32 and ISIZE)
    let split_pos = input.len() - 8;
    let (compressed_data, trailing) = input.split_at(split_pos);
    
    let (trailing, crc32) = le_u32(trailing)?;
    let (trailing, isize) = le_u32(trailing)?;

    Ok((
        trailing,
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
        Ok((_, gzip_file)) => {
            println!("GZIP File Structure:");
            println!("Header: {:#?}", gzip_file.header);
            println!("Compressed data length: {} bytes", gzip_file.compressed_data.len());
            println!("CRC32: {:08x}", gzip_file.crc32);
            println!("Original size: {} bytes", gzip_file.isize);
        }
        Err(e) => eprintln!("Error parsing GZIP file: {:?}", e),
    }

    Ok(())
}