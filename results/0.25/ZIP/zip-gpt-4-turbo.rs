use nom::{
    bytes::complete::{take, tag},
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct LocalFileHeader {
    version_needed: u16,
    general_purpose_bit_flag: u16,
    compression_method: u16,
    last_mod_file_time: u16,
    last_mod_file_date: u16,
    crc_32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_name: Vec<u8>,
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
    crc_32: u32,
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
struct EndOfCentralDirectoryRecord {
    number_of_this_disk: u16,
    number_of_the_disk_with_the_start_of_the_central_directory: u16,
    total_number_of_entries_in_the_central_directory_on_this_disk: u16,
    total_number_of_entries_in_the_central_directory: u16,
    size_of_the_central_directory: u32,
    offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number: u32,
    zip_file_comment_length: u16,
    zip_file_comment: Vec<u8>,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, _) = tag("PK\x03\x04")(input)?;
    let (input, (version_needed, general_purpose_bit_flag, compression_method, last_mod_file_time, last_mod_file_date, crc_32, compressed_size, uncompressed_size, file_name_length, extra_field_length)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16))(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;

    Ok((
        input,
        LocalFileHeader {
            version_needed,
            general_purpose_bit_flag,
            compression_method,
            last_mod_file_time,
            last_mod_file_date,
            crc_32,
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
    let (input, _) = tag("PK\x01\x02")(input)?;
    let (input, (version_made_by, version_needed, general_purpose_bit_flag, compression_method, last_mod_file_time, last_mod_file_date, crc_32, compressed_size, uncompressed_size, file_name_length, extra_field_length, file_comment_length, disk_number_start, internal_file_attributes, external_file_attributes, relative_offset_of_local_header)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32))(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    Ok((
        input,
        CentralDirectoryFileHeader {
            version_made_by,
            version_needed,
            general_purpose_bit_flag,
            compression_method,
            last_mod_file_time,
            last_mod_file_date,
            crc_32,
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

fn parse_end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (input, _) = tag("PK\x05\x06")(input)?;
    let (input, (number_of_this_disk, number_of_the_disk_with_the_start_of_the_central_directory, total_number_of_entries_in_the_central_directory_on_this_disk, total_number_of_entries_in_the_central_directory, size_of_the_central_directory, offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number, zip_file_comment_length)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u16))(input)?;
    let (input, zip_file_comment) = take(zip_file_comment_length)(input)?;

    Ok((
        input,
        EndOfCentralDirectoryRecord {
            number_of_this_disk,
            number_of_the_disk_with_the_start_of_the_central_directory,
            total_number_of_entries_in_the_central_directory_on_this_disk,
            total_number_of_entries_in_the_central_directory,
            size_of_the_central_directory,
            offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number,
            zip_file_comment_length,
            zip_file_comment: zip_file_comment.to_vec(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No file specified"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let (_, local_file_header) = parse_local_file_header(&buffer).unwrap();
    println!("{:?}", local_file_header);

    Ok(())
}