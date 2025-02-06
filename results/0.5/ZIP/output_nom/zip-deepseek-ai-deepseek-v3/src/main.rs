use nom::{
    bytes::complete::take,
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct LocalFileHeader {
    signature: u32,
    version_needed: u16,
    bit_flag: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
}

#[derive(Debug)]
struct CentralDirectoryFileHeader {
    signature: u32,
    version_made_by: u16,
    version_needed: u16,
    bit_flag: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    internal_file_attributes: u16,
    external_file_attributes: u32,
    relative_offset: u32,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    signature: u32,
    disk_number: u16,
    central_directory_disk: u16,
    num_records_on_disk: u16,
    total_records: u16,
    central_directory_size: u32,
    central_directory_offset: u32,
    zip_file_comment_length: u16,
    zip_file_comment: Vec<u8>,
}

#[derive(Debug)]
struct DataDescriptor {
    signature: u32,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, signature) = le_u32(input)?;
    let (input, version_needed) = le_u16(input)?;
    let (input, bit_flag) = le_u16(input)?;
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
            signature,
            version_needed,
            bit_flag,
            compression_method,
            last_mod_time,
            last_mod_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name_length,
            extra_field_length,
            file_name: file_name.to_vec(),
            extra_field: extra_field.to_vec(),
        },
    ))
}

fn parse_central_directory_file_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryFileHeader> {
    let (input, signature) = le_u32(input)?;
    let (input, version_made_by) = le_u16(input)?;
    let (input, version_needed) = le_u16(input)?;
    let (input, bit_flag) = le_u16(input)?;
    let (input, compression_method) = le_u16(input)?;
    let (input, last_mod_time) = le_u16(input)?;
    let (input, last_mod_date) = le_u16(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, compressed_size) = le_u32(input)?;
    let (input, uncompressed_size) = le_u32(input)?;
    let (input, file_name_length) = le_u16(input)?;
    let (input, extra_field_length) = le_u16(input)?;
    let (input, file_comment_length) = le_u16(input)?;
    let (input, disk_number_start) = le_u16(input)?;
    let (input, internal_file_attributes) = le_u16(input)?;
    let (input, external_file_attributes) = le_u32(input)?;
    let (input, relative_offset) = le_u32(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    Ok((
        input,
        CentralDirectoryFileHeader {
            signature,
            version_made_by,
            version_needed,
            bit_flag,
            compression_method,
            last_mod_time,
            last_mod_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name_length,
            extra_field_length,
            file_comment_length,
            disk_number_start,
            internal_file_attributes,
            external_file_attributes,
            relative_offset,
            file_name: file_name.to_vec(),
            extra_field: extra_field.to_vec(),
            file_comment: file_comment.to_vec(),
        },
    ))
}

fn parse_end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (input, signature) = le_u32(input)?;
    let (input, disk_number) = le_u16(input)?;
    let (input, central_directory_disk) = le_u16(input)?;
    let (input, num_records_on_disk) = le_u16(input)?;
    let (input, total_records) = le_u16(input)?;
    let (input, central_directory_size) = le_u32(input)?;
    let (input, central_directory_offset) = le_u32(input)?;
    let (input, zip_file_comment_length) = le_u16(input)?;
    let (input, zip_file_comment) = take(zip_file_comment_length)(input)?;

    Ok((
        input,
        EndOfCentralDirectoryRecord {
            signature,
            disk_number,
            central_directory_disk,
            num_records_on_disk,
            total_records,
            central_directory_size,
            central_directory_offset,
            zip_file_comment_length,
            zip_file_comment: zip_file_comment.to_vec(),
        },
    ))
}

fn parse_data_descriptor(input: &[u8]) -> IResult<&[u8], DataDescriptor> {
    let (input, signature) = le_u32(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, compressed_size) = le_u32(input)?;
    let (input, uncompressed_size) = le_u32(input)?;

    Ok((
        input,
        DataDescriptor {
            signature,
            crc32,
            compressed_size,
            uncompressed_size,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <zip file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    if let Ok((_, local_file_header)) = parse_local_file_header(&data) {
        println!("{:?}", local_file_header);
    }

    if let Ok((_, central_dir_header)) = parse_central_directory_file_header(&data) {
        println!("{:?}", central_dir_header);
    }

    if let Ok((_, eocd_record)) = parse_end_of_central_directory_record(&data) {
        println!("{:?}", eocd_record);
    }

    if let Ok((_, data_descriptor)) = parse_data_descriptor(&data) {
        println!("{:?}", data_descriptor);
    }
}