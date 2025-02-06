use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    error::ErrorKind,
    multi::{count, many0, many_m_n},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
    Parser,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct ZipCentralDirectoryHeader {
    version_made_by: u16,
    version_needed: u16,
    general_bit_flag: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    internal_file_attributes: u16,
    external_file_attributes: u32,
    local_header_offset: u32,
    filename: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

fn parse_central_directory_header(input: &[u8]) -> IResult<&[u8], ZipCentralDirectoryHeader> {
    let (input, _) = tag(&[0x50, 0x4B, 0x01, 0x02])(input)?;
    
    let (input, (
        version_made_by,
        version_needed,
        general_bit_flag,
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_length,
        extra_field_length,
        file_comment_length,
        disk_number_start,
        internal_file_attributes,
        external_file_attributes,
        local_header_offset
    )) = tuple((
        le_u16, le_u16, le_u16, le_u16, le_u16, le_u16,
        le_u32, le_u32, le_u32, le_u16, le_u16, le_u16,
        le_u16, le_u16, le_u32, le_u32
    ))(input)?;

    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    Ok((input, ZipCentralDirectoryHeader {
        version_made_by,
        version_needed,
        general_bit_flag,
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_length,
        extra_field_length,
        file_comment_length,
        disk_number_start,
        internal_file_attributes,
        external_file_attributes,
        local_header_offset,
        filename: filename.to_vec(),
        extra_field: extra_field.to_vec(),
        file_comment: file_comment.to_vec(),
    }))
}

fn parse_zip_archive(input: &[u8]) -> IResult<&[u8], Vec<ZipCentralDirectoryHeader>> {
    many0(parse_central_directory_header)(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        std::process::exit(1);
    }

    let file_contents = fs::read(&args[1])?;
    match parse_zip_archive(&file_contents) {
        Ok((_, headers)) => {
            println!("Parsed {} ZIP central directory headers", headers.len());
            for header in headers {
                println!("{:?}", header);
            }
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1)
        }
    }
}