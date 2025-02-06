use nom::{
    bytes::complete::{tag, take, take_while_m_n},
    combinator::{map, map_res, opt, verify},
    error::ErrorKind,
    number::complete::{le_u16, le_u32, le_u64},
    sequence::{tuple, preceded},
    IResult,
};
use std::fs;
use std::path::Path;
use std::str;

#[derive(Debug)]
struct LocalFileHeader {
    signature: u32,
    version_needed: u16,
    general_purpose_bit_flag: u16,
    compression_method: u16,
    last_modified_time: u16,
    last_modified_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    filename: String,
    extra_field: Vec<u8>,
}

#[derive(Debug)]
struct CentralDirectoryHeader {
    signature: u32,
    version_made_by: u16,
    version_needed: u16,
    general_purpose_bit_flag: u16,
    compression_method: u16,
    last_modified_time: u16,
    last_modified_date: u16,
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
    filename: String,
    extra_field: Vec<u8>,
    file_comment: String,
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    signature: u32,
    number_of_this_disk: u16,
    number_of_disk_with_central_directory: u16,
    total_number_of_entries_in_central_directory_on_this_disk: u16,
    total_number_of_entries_in_central_directory: u16,
    size_of_central_directory: u32,
    offset_of_start_of_central_directory: u32,
    zip_file_comment_length: u16,
    zip_file_comment: String,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, signature) = le_u32(input)?;
    let (input, version_needed) = le_u16(input)?;
    let (input, general_purpose_bit_flag) = le_u16(input)?;
    let (input, compression_method) = le_u16(input)?;
    let (input, last_modified_time) = le_u16(input)?;
    let (input, last_modified_date) = le_u16(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, compressed_size) = le_u32(input)?;
    let (input, uncompressed_size) = le_u32(input)?;
    let (input, filename_length) = le_u16(input)?;
    let (input, extra_field_length) = le_u16(input)?;
    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let filename_str = match str::from_utf8(filename) {
        Ok(s) => s.to_string(),
        Err(_) => String::from(""), // Handle invalid UTF-8
    };
    Ok((
        input,
        LocalFileHeader {
            signature,
            version_needed,
            general_purpose_bit_flag,
            compression_method,
            last_modified_time,
            last_modified_date,
            crc32,
            compressed_size,
            uncompressed_size,
            filename_length,
            extra_field_length,
            filename: filename_str,
            extra_field: extra_field.to_vec(),
        },
    ))
}


fn parse_central_directory_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryHeader> {
    let (input, signature) = le_u32(input)?;
    let (input, version_made_by) = le_u16(input)?;
    let (input, version_needed) = le_u16(input)?;
    let (input, general_purpose_bit_flag) = le_u16(input)?;
    let (input, compression_method) = le_u16(input)?;
    let (input, last_modified_time) = le_u16(input)?;
    let (input, last_modified_date) = le_u16(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, compressed_size) = le_u32(input)?;
    let (input, uncompressed_size) = le_u32(input)?;
    let (input, filename_length) = le_u16(input)?;
    let (input, extra_field_length) = le_u16(input)?;
    let (input, file_comment_length) = le_u16(input)?;
    let (input, disk_number_start) = le_u16(input)?;
    let (input, internal_file_attributes) = le_u16(input)?;
    let (input, external_file_attributes) = le_u32(input)?;
    let (input, local_header_offset) = le_u32(input)?;
    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    let filename_str = match str::from_utf8(filename) {
        Ok(s) => s.to_string(),
        Err(_) => String::from(""),
    };
    let file_comment_str = match str::from_utf8(file_comment) {
        Ok(s) => s.to_string(),
        Err(_) => String::from(""),
    };

    Ok((
        input,
        CentralDirectoryHeader {
            signature,
            version_made_by,
            version_needed,
            general_purpose_bit_flag,
            compression_method,
            last_modified_time,
            last_modified_date,
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
            filename: filename_str,
            extra_field: extra_field.to_vec(),
            file_comment: file_comment_str,
        },
    ))
}

fn parse_end_of_central_directory_record(
    input: &[u8],
) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (input, signature) = le_u32(input)?;
    let (input, number_of_this_disk) = le_u16(input)?;
    let (input, number_of_disk_with_central_directory) = le_u16(input)?;
    let (input, total_number_of_entries_in_central_directory_on_this_disk) = le_u16(input)?;
    let (input, total_number_of_entries_in_central_directory) = le_u16(input)?;
    let (input, size_of_central_directory) = le_u32(input)?;
    let (input, offset_of_start_of_central_directory) = le_u32(input)?;
    let (input, zip_file_comment_length) = le_u16(input)?;
    let (input, zip_file_comment) = take(zip_file_comment_length)(input)?;
    let zip_file_comment_str = match str::from_utf8(zip_file_comment) {
        Ok(s) => s.to_string(),
        Err(_) => String::from(""),
    };
    Ok((
        input,
        EndOfCentralDirectoryRecord {
            signature,
            number_of_this_disk,
            number_of_disk_with_central_directory,
            total_number_of_entries_in_central_directory_on_this_disk,
            total_number_of_entries_in_central_directory,
            size_of_central_directory,
            offset_of_start_of_central_directory,
            zip_file_comment_length,
            zip_file_comment: zip_file_comment_str,
        },
    ))
}

fn parse_zip_file(input: &[u8]) -> IResult<&[u8], Vec<CentralDirectoryHeader>> {
    let (input, _) = take(input.len() - 22)(input)?; //Skip to the end minus EOCD size
    let (input, eocd) = parse_end_of_central_directory_record(input)?;
    let central_dir_start = input.len() - (eocd.size_of_central_directory as usize) - 22;
    let central_dir_data = &input[central_dir_start..input.len() - 22];
    let mut central_directory = Vec::new();
    let mut remaining = central_dir_data;
    for _ in 0..eocd.total_number_of_entries_in_central_directory {
        match parse_central_directory_header(remaining) {
            Ok((rest, header)) => {
                central_directory.push(header);
                remaining = rest;
            }
            Err(e) => return Err(e),
        }
    }
    Ok((input, central_directory))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: zip_parser <zip_file>");
        return;
    }

    let path = Path::new(&args[1]);
    let zip_data = match fs::read(path) {
        Ok(data) => data,
        Err(e) => {
            println!("Error reading file: {}", e);
            return;
        }
    };

    match parse_zip_file(&zip_data) {
        Ok((_, central_directory)) => {
            println!("Central Directory Entries:");
            for entry in central_directory {
                println!("{:?}", entry);
            }
        }
        Err(e) => {
            println!("Error parsing ZIP file: {:?}", e);
        }
    }
}
