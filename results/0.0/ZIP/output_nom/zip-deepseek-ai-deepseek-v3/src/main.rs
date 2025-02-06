use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u64, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct LocalFileHeader {
    signature: u32,
    version_needed: u16,
    general_purpose_bit: u16,
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
    general_purpose_bit: u16,
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
    internal_file_attr: u16,
    external_file_attr: u32,
    relative_offset: u32,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    signature: u32,
    disk_number: u16,
    disk_start: u16,
    num_entries_on_disk: u16,
    total_entries: u16,
    central_dir_size: u32,
    central_dir_offset: u32,
    comment_length: u16,
    comment: Vec<u8>,
}

#[derive(Debug)]
struct Zip64EndOfCentralDirectoryRecord {
    signature: u32,
    size_of_record: u64,
    version_made_by: u16,
    version_needed: u16,
    disk_number: u32,
    disk_start: u32,
    num_entries_on_disk: u64,
    total_entries: u64,
    central_dir_size: u64,
    central_dir_offset: u64,
}

#[derive(Debug)]
struct Zip64EndOfCentralDirectoryLocator {
    signature: u32,
    disk_number: u32,
    eocd_offset: u64,
    total_disks: u32,
}

#[derive(Debug)]
struct DataDescriptor {
    signature: u32,
    crc32: u32,
    compressed_size: u64,
    uncompressed_size: u64,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, (signature, version_needed, general_purpose_bit, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length)) = tuple((le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16))(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    Ok((input, LocalFileHeader {
        signature,
        version_needed,
        general_purpose_bit,
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
    }))
}

fn parse_central_directory_file_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryFileHeader> {
    let (input, (signature, version_made_by, version_needed, general_purpose_bit, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length, file_comment_length, disk_number_start, internal_file_attr, external_file_attr, relative_offset)) = tuple((le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32))(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;
    Ok((input, CentralDirectoryFileHeader {
        signature,
        version_made_by,
        version_needed,
        general_purpose_bit,
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
        internal_file_attr,
        external_file_attr,
        relative_offset,
        file_name: file_name.to_vec(),
        extra_field: extra_field.to_vec(),
        file_comment: file_comment.to_vec(),
    }))
}

fn parse_end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (input, (signature, disk_number, disk_start, num_entries_on_disk, total_entries, central_dir_size, central_dir_offset, comment_length)) = tuple((le_u32, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u16))(input)?;
    let (input, comment) = take(comment_length)(input)?;
    Ok((input, EndOfCentralDirectoryRecord {
        signature,
        disk_number,
        disk_start,
        num_entries_on_disk,
        total_entries,
        central_dir_size,
        central_dir_offset,
        comment_length,
        comment: comment.to_vec(),
    }))
}

fn parse_zip64_end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], Zip64EndOfCentralDirectoryRecord> {
    let (input, (signature, size_of_record, version_made_by, version_needed, disk_number, disk_start, num_entries_on_disk, total_entries, central_dir_size, central_dir_offset)) = tuple((le_u32, le_u64, le_u16, le_u16, le_u32, le_u32, le_u64, le_u64, le_u64, le_u64))(input)?;
    Ok((input, Zip64EndOfCentralDirectoryRecord {
        signature,
        size_of_record,
        version_made_by,
        version_needed,
        disk_number,
        disk_start,
        num_entries_on_disk,
        total_entries,
        central_dir_size,
        central_dir_offset,
    }))
}

fn parse_zip64_end_of_central_directory_locator(input: &[u8]) -> IResult<&[u8], Zip64EndOfCentralDirectoryLocator> {
    let (input, (signature, disk_number, eocd_offset, total_disks)) = tuple((le_u32, le_u32, le_u64, le_u32))(input)?;
    Ok((input, Zip64EndOfCentralDirectoryLocator {
        signature,
        disk_number,
        eocd_offset,
        total_disks,
    }))
}

fn parse_data_descriptor(input: &[u8]) -> IResult<&[u8], DataDescriptor> {
    let (input, (signature, crc32, compressed_size, uncompressed_size)) = tuple((le_u32, le_u32, le_u64, le_u64))(input)?;
    Ok((input, DataDescriptor {
        signature,
        crc32,
        compressed_size,
        uncompressed_size,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    if let Ok((_, local_file_header)) = parse_local_file_header(&data) {
        println!("{:?}", local_file_header);
    }

    if let Ok((_, central_directory_file_header)) = parse_central_directory_file_header(&data) {
        println!("{:?}", central_directory_file_header);
    }

    if let Ok((_, end_of_central_directory_record)) = parse_end_of_central_directory_record(&data) {
        println!("{:?}", end_of_central_directory_record);
    }

    if let Ok((_, zip64_end_of_central_directory_record)) = parse_zip64_end_of_central_directory_record(&data) {
        println!("{:?}", zip64_end_of_central_directory_record);
    }

    if let Ok((_, zip64_end_of_central_directory_locator)) = parse_zip64_end_of_central_directory_locator(&data) {
        println!("{:?}", zip64_end_of_central_directory_locator);
    }

    if let Ok((_, data_descriptor)) = parse_data_descriptor(&data) {
        println!("{:?}", data_descriptor);
    }
}