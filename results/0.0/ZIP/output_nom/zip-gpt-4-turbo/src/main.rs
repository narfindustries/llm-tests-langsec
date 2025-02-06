use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::count,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct LocalFileHeader {
    version_needed: u16,
    general_purpose_bit_flag: u16,
    compression_method: u16,
    last_mod_file_time: u16,
    last_mod_file_date: u16,
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
    general_purpose_bit_flag: u16,
    compression_method: u16,
    last_mod_file_time: u16,
    last_mod_file_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name: String,
    extra_field: Vec<u8>,
    file_comment: String,
    disk_number_start: u16,
    internal_file_attributes: u16,
    external_file_attributes: u32,
    relative_offset_of_local_header: u32,
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    number_of_this_disk: u16,
    disk_where_central_directory_starts: u16,
    number_of_central_directory_records_on_this_disk: u16,
    total_number_of_central_directory_records: u16,
    size_of_central_directory: u32,
    offset_of_start_of_central_directory: u32,
    zip_file_comment: String,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, (_, version_needed, general_purpose_bit_flag, compression_method, last_mod_file_time, last_mod_file_date, crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length)) =
        tuple((tag(b"PK\x03\x04"), le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16))(input)?;

    let (input, file_name) = map_res(take(file_name_length), std::str::from_utf8)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;

    Ok((
        input,
        LocalFileHeader {
            version_needed,
            general_purpose_bit_flag,
            compression_method,
            last_mod_file_time,
            last_mod_file_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name: file_name.to_string(),
            extra_field: extra_field.to_vec(),
        },
    ))
}

fn parse_central_directory_file_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryFileHeader> {
    let (input, (_, version_made_by, version_needed, general_purpose_bit_flag, compression_method, last_mod_file_time, last_mod_file_date, crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length, file_comment_length, disk_number_start, internal_file_attributes, external_file_attributes, relative_offset_of_local_header)) =
        tuple((tag(b"PK\x01\x02"), le_u16, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32))(input)?;

    let (input, file_name) = map_res(take(file_name_length), std::str::from_utf8)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = map_res(take(file_comment_length), std::str::from_utf8)(input)?;

    Ok((
        input,
        CentralDirectoryFileHeader {
            version_made_by,
            version_needed,
            general_purpose_bit_flag,
            compression_method,
            last_mod_file_time,
            last_mod_file_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name: file_name.to_string(),
            extra_field: extra_field.to_vec(),
            file_comment: file_comment.to_string(),
            disk_number_start,
            internal_file_attributes,
            external_file_attributes,
            relative_offset_of_local_header,
        },
    ))
}

fn parse_end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (input, (_, number_of_this_disk, disk_where_central_directory_starts, number_of_central_directory_records_on_this_disk, total_number_of_central_directory_records, size_of_central_directory, offset_of_start_of_central_directory, zip_file_comment_length)) =
        tuple((tag(b"PK\x05\x06"), le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u16))(input)?;

    let (input, zip_file_comment) = map_res(take(zip_file_comment_length), std::str::from_utf8)(input)?;

    Ok((
        input,
        EndOfCentralDirectoryRecord {
            number_of_this_disk,
            disk_where_central_directory_starts,
            number_of_central_directory_records_on_this_disk,
            total_number_of_central_directory_records,
            size_of_central_directory,
            offset_of_start_of_central_directory,
            zip_file_comment: zip_file_comment.to_string(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <zip_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let (_, local_file_header) = parse_local_file_header(&buffer).unwrap();
    println!("{:?}", local_file_header);

    Ok(())
}