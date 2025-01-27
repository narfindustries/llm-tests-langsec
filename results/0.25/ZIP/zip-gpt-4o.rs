extern crate nom;

use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt},
    multi::count,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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
    let (input, _) = tag([0x50, 0x4b, 0x03, 0x04])(input)?;
    let (input, (version, flags, compression, mod_time, mod_date, crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16))(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_data) = take(compressed_size)(input)?;

    Ok((
        input,
        LocalFileHeader {
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
        },
    ))
}

#[derive(Debug)]
struct CentralDirectoryFileHeader {
    version_made_by: u16,
    version_needed: u16,
    flags: u16,
    compression: u16,
    mod_time: u16,
    mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    internal_file_attributes: u16,
    external_file_attributes: u32,
    local_header_offset: u32,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

fn parse_central_directory_file_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryFileHeader> {
    let (input, _) = tag([0x50, 0x4b, 0x01, 0x02])(input)?;
    let (input, (version_made_by, version_needed, flags, compression, mod_time, mod_date, crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length, file_comment_length, disk_number_start, internal_file_attributes, external_file_attributes, local_header_offset)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32))(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    Ok((
        input,
        CentralDirectoryFileHeader {
            version_made_by,
            version_needed,
            flags,
            compression,
            mod_time,
            mod_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name_length,
            extra_field_length,
            file_comment_length,
            disk_number_start,
            internal_file_attributes,
            external_file_attributes,
            local_header_offset,
            file_name: file_name.to_vec(),
            extra_field: extra_field.to_vec(),
            file_comment: file_comment.to_vec(),
        },
    ))
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    disk_number: u16,
    central_directory_disk_number: u16,
    central_directory_records_on_this_disk: u16,
    total_central_directory_records: u16,
    central_directory_size: u32,
    central_directory_offset: u32,
    comment_length: u16,
    comment: Vec<u8>,
}

fn parse_end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (input, _) = tag([0x50, 0x4b, 0x05, 0x06])(input)?;
    let (input, (disk_number, central_directory_disk_number, central_directory_records_on_this_disk, total_central_directory_records, central_directory_size, central_directory_offset, comment_length)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u16))(input)?;
    let (input, comment) = take(comment_length)(input)?;

    Ok((
        input,
        EndOfCentralDirectoryRecord {
            disk_number,
            central_directory_disk_number,
            central_directory_records_on_this_disk,
            total_central_directory_records,
            central_directory_size,
            central_directory_offset,
            comment_length,
            comment: comment.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip-file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let mut input = &buffer[..];

    while !input.is_empty() {
        if let Ok((remaining, header)) = parse_local_file_header(input) {
            println!("{:?}", header);
            input = remaining;
        } else if let Ok((remaining, header)) = parse_central_directory_file_header(input) {
            println!("{:?}", header);
            input = remaining;
        } else if let Ok((remaining, record)) = parse_end_of_central_directory_record(input) {
            println!("{:?}", record);
            input = remaining;
        } else {
            eprintln!("Failed to parse ZIP file");
            break;
        }
    }
}