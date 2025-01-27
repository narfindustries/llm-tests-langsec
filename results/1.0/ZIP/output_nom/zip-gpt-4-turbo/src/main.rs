use nom::{
    bytes::complete::{take, take_until},
    combinator::{map, map_res},
    multi::count,
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug)]
struct ZipLocalFileHeader {
    version_needed: u16,
    general_purpose_bit_flag: u16,
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
struct ZipCentralDirHeader {
    version_made_by: u16,
    version_needed: u16,
    general_purpose_bit_flag: u16,
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
    relative_offset_of_local_header: u32,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

#[derive(Debug)]
struct ZipEndOfCentralDirectoryRecord {
    disk_number: u16,
    disk_start: u16,
    number_of_records_this_disk: u16,
    total_number_of_records: u16,
    size_of_central_directory: u32,
    offset_of_start_central_directory: u32,
    zip_comment_length: u16,
    zip_comment: Vec<u8>,
}

fn parse_zip_local_file_header(input: &[u8]) -> IResult<&[u8], ZipLocalFileHeader> {
    let (input, _) = take_until("PK\x03\x04")(input)?;
    let (input, _) = take(4usize)(input)?;
    let (input, version_needed) = le_u16(input)?;
    let (input, general_purpose_bit_flag) = le_u16(input)?;
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
        ZipLocalFileHeader {
            version_needed,
            general_purpose_bit_flag,
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

fn parse_central_directory_header(input: &[u8]) -> IResult<&[u8], ZipCentralDirHeader> {
    let (input, _) = take_until("PK\x01\x02")(input)?;
    let (input, _) = take(4usize)(input)?;
    let (input, version_made_by) = le_u16(input)?;
    let (input, version_needed) = le_u16(input)?;
    let (input, general_purpose_bit_flag) = le_u16(input)?;
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
    let (input, relative_offset_of_local_header) = le_u32(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    Ok((
        input,
        ZipCentralDirHeader {
            version_made_by,
            version_needed,
            general_purpose_bit_flag,
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
            relative_offset_of_local_header,
            file_name: file_name.to_vec(),
            extra_field: extra_field.to_vec(),
            file_comment: file_comment.to_vec(),
        },
    ))
}

fn parse_end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], ZipEndOfCentralDirectoryRecord> {
    let (input, _) = take_until("PK\x05\x06")(input)?;
    let (input, _) = take(4usize)(input)?;
    let (input, disk_number) = le_u16(input)?;
    let (input, disk_start) = le_u16(input)?;
    let (input, number_of_records_this_disk) = le_u16(input)?;
    let (input, total_number_of_records) = le_u16(input)?;
    let (input, size_of_central_directory) = le_u32(input)?;
    let (input, offset_of_start_central_directory) = le_u32(input)?;
    let (input, zip_comment_length) = le_u16(input)?;
    let (input, zip_comment) = take(zip_comment_length)(input)?;

    Ok((
        input,
        ZipEndOfCentralDirectoryRecord {
            disk_number,
            disk_start,
            number_of_records_this_disk,
            total_number_of_records,
            size_of_central_directory,
            offset_of_start_central_directory,
            zip_comment_length,
            zip_comment: zip_comment.to_vec(),
        },
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip-file>", args[0]);
        return Ok(());
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    let (_, local_header) = parse_zip_local_file_header(&data).unwrap();
    println!("{:?}", local_header);

    let (_, central_header) = parse_central_directory_header(&data).unwrap();
    println!("{:?}", central_header);

    let (_, end_record) = parse_end_of_central_directory_record(&data).unwrap();
    println!("{:?}", end_record);

    Ok(())
}