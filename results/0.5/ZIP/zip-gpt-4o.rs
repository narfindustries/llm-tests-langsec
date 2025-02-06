use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;
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
    file_name: Vec<u8>,
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
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
    disk_number_start: u16,
    internal_attrs: u16,
    external_attrs: u32,
    local_header_offset: u32,
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    disk_number: u16,
    central_dir_start_disk: u16,
    num_records_this_disk: u16,
    total_num_records: u16,
    central_dir_size: u32,
    central_dir_offset: u32,
    zip_file_comment: Vec<u8>,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, _) = tag([0x50, 0x4b, 0x03, 0x04])(input)?;
    let (input, (version_needed, general_purpose, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16))(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;

    Ok((input, LocalFileHeader {
        version_needed,
        general_purpose,
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        file_name: file_name.to_vec(),
        extra_field: extra_field.to_vec(),
    }))
}

fn parse_central_directory_file_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryFileHeader> {
    let (input, _) = tag([0x50, 0x4b, 0x01, 0x02])(input)?;
    let (input, (version_made_by, version_needed, general_purpose, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length, file_comment_length, disk_number_start, internal_attrs, external_attrs, local_header_offset)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32))(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    Ok((input, CentralDirectoryFileHeader {
        version_made_by,
        version_needed,
        general_purpose,
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        file_name: file_name.to_vec(),
        extra_field: extra_field.to_vec(),
        file_comment: file_comment.to_vec(),
        disk_number_start,
        internal_attrs,
        external_attrs,
        local_header_offset,
    }))
}

fn parse_end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (input, _) = tag([0x50, 0x4b, 0x05, 0x06])(input)?;
    let (input, (disk_number, central_dir_start_disk, num_records_this_disk, total_num_records, central_dir_size, central_dir_offset, zip_file_comment_length)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u16))(input)?;
    let (input, zip_file_comment) = take(zip_file_comment_length)(input)?;

    Ok((input, EndOfCentralDirectoryRecord {
        disk_number,
        central_dir_start_disk,
        num_records_this_disk,
        total_num_records,
        central_dir_size,
        central_dir_offset,
        zip_file_comment: zip_file_comment.to_vec(),
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path).expect("Failed to open file");

    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let mut input = &buffer[..];

    while !input.is_empty() {
        if let Ok((rest, header)) = parse_local_file_header(input) {
            println!("Local File Header: {:?}", header);
            input = rest;
        } else if let Ok((rest, header)) = parse_central_directory_file_header(input) {
            println!("Central Directory File Header: {:?}", header);
            input = rest;
        } else if let Ok((rest, record)) = parse_end_of_central_directory_record(input) {
            println!("End of Central Directory Record: {:?}", record);
            input = rest;
        } else {
            break;
        }
    }
}