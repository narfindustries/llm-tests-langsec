// Note: This is a simplified example and may not cover all edge cases or optional fields in the ZIP specification.

use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct LocalFileHeader {
    version_needed: u16,
    general_purpose: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name: String,
    extra_field: Vec<u8>,
}

#[derive(Debug)]
struct CentralDirectoryFileHeader {
    version_made_by: u16,
    version_needed: u16,
    general_purpose: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name: String,
    extra_field: Vec<u8>,
    file_comment: String,
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    number_of_this_disk: u16,
    disk_where_cd_starts: u16,
    number_of_cd_records_on_this_disk: u16,
    total_number_of_cd_records: u16,
    size_of_cd: u32,
    offset_of_start_of_cd: u32,
    zip_file_comment: String,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, _) = tag([0x50, 0x4b, 0x03, 0x04])(input)?;
    let (input, version_needed) = le_u16(input)?;
    let (input, general_purpose) = le_u16(input)?;
    let (input, compression_method) = le_u16(input)?;
    let (input, last_mod_time) = le_u16(input)?;
    let (input, last_mod_date) = le_u16(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, compressed_size) = le_u32(input)?;
    let (input, uncompressed_size) = le_u32(input)?;
    let (input, file_name_length) = le_u16(input)?;
    let (input, extra_field_length) = le_u16(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;

    Ok((
        input,
        LocalFileHeader {
            version_needed,
            general_purpose,
            compression_method,
            last_mod_time,
            last_mod_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name: String::from_utf8_lossy(file_name).to_string(),
            extra_field: extra_field.to_vec(),
        },
    ))
}

fn parse_central_directory_file_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryFileHeader> {
    let (input, _) = tag([0x50, 0x4b, 0x01, 0x02])(input)?;
    let (input, version_made_by) = le_u16(input)?;
    let (input, version_needed) = le_u16(input)?;
    let (input, general_purpose) = le_u16(input)?;
    let (input, compression_method) = le_u16(input)?;
    let (input, last_mod_time) = le_u16(input)?;
    let (input, last_mod_date) = le_u16(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, compressed_size) = le_u32(input)?;
    let (input, uncompressed_size) = le_u32(input)?;
    let (input, file_name_length) = le_u16(input)?;
    let (input, extra_field_length) = le_u16(input)?;
    let (input, file_comment_length) = le_u16(input)?;
    let (input, _disk_number_start) = le_u16(input)?;
    let (input, _internal_file_attributes) = le_u16(input)?;
    let (input, _external_file_attributes) = le_u32(input)?;
    let (input, _relative_offset_of_local_header) = le_u32(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    Ok((
        input,
        CentralDirectoryFileHeader {
            version_made_by,
            version_needed,
            general_purpose,
            compression_method,
            last_mod_time,
            last_mod_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name: String::from_utf8_lossy(file_name).to_string(),
            extra_field: extra_field.to_vec(),
            file_comment: String::from_utf8_lossy(file_comment).to_string(),
        },
    ))
}

fn parse_end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (input, _) = tag([0x50, 0x4b, 0x05, 0x06])(input)?;
    let (input, number_of_this_disk) = le_u16(input)?;
    let (input, disk_where_cd_starts) = le_u16(input)?;
    let (input, number_of_cd_records_on_this_disk) = le_u16(input)?;
    let (input, total_number_of_cd_records) = le_u16(input)?;
    let (input, size_of_cd) = le_u32(input)?;
    let (input, offset_of_start_of_cd) = le_u32(input)?;
    let (input, zip_file_comment_length) = le_u16(input)?;
    let (input, zip_file_comment) = take(zip_file_comment_length)(input)?;

    Ok((
        input,
        EndOfCentralDirectoryRecord {
            number_of_this_disk,
            disk_where_cd_starts,
            number_of_cd_records_on_this_disk,
            total_number_of_cd_records,
            size_of_cd,
            offset_of_start_of_cd,
            zip_file_comment: String::from_utf8_lossy(zip_file_comment).to_string(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zipfile>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let mut input = &buffer[..];

    while !input.is_empty() {
        if let Ok((remaining, local_file_header)) = parse_local_file_header(input) {
            println!("{:?}", local_file_header);
            input = remaining;
        } else if let Ok((remaining, central_directory_file_header)) = parse_central_directory_file_header(input) {
            println!("{:?}", central_directory_file_header);
            input = remaining;
        } else if let Ok((remaining, end_of_central_directory_record)) = parse_end_of_central_directory_record(input) {
            println!("{:?}", end_of_central_directory_record);
            input = remaining;
        } else {
            break;
        }
    }
}