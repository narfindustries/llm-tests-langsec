use nom::bytes::complete::{take, tag};
use nom::number::complete::{le_u16, le_u32, le_u8};
use nom::combinator::map;
use nom::sequence::tuple;
use nom::multi::count;
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

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
    filename_len: u16,
    extra_field_len: u16,
    filename: Vec<u8>,
    extra_field: Vec<u8>,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, (_, version, flags, compression, mod_time, mod_date, crc32, compressed_size, uncompressed_size, filename_len, extra_field_len)) = 
        tuple((
            tag([0x50, 0x4B, 0x03, 0x04]), // Local file header signature
            le_u16, // Version needed to extract
            le_u16, // General purpose bit flag
            le_u16, // Compression method
            le_u16, // Last mod file time
            le_u16, // Last mod file date
            le_u32, // CRC-32
            le_u32, // Compressed size
            le_u32, // Uncompressed size
            le_u16, // File name length
            le_u16, // Extra field length
        ))(input)?;
    
    let (input, filename) = take(filename_len)(input)?;
    let (input, extra_field) = take(extra_field_len)(input)?;

    let header = LocalFileHeader {
        version,
        flags,
        compression,
        mod_time,
        mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_len,
        extra_field_len,
        filename: filename.to_vec(),
        extra_field: extra_field.to_vec(),
    };

    Ok((input, header))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input_zip_file>", args[0]);
        return Ok(());
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_local_file_header(&buffer) {
        Ok((_remaining, header)) => {
            println!("Parsed Local File Header: {:?}", header);
        }
        Err(e) => {
            eprintln!("Failed to parse the ZIP file: {:?}", e);
        }
    }

    Ok(())
}