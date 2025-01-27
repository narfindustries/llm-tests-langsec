use std::fs::File;
use std::io::{Read, Error};
use std::env;
use nom::{
    IResult,
    bytes::complete::{take, take_while_m_n},
    number::complete::{be_u16, be_u32, be_u64},
};

#[derive(Debug)]
struct ZipLocalFileHeader {
    local_file_header_signature: u32,
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: u16,
    last_modification_time: u16,
    last_modification_date: u16,
    crc_32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
}

impl ZipLocalFileHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, local_file_header_signature) = be_u32(input)?;
        let (input, version_needed_to_extract) = be_u16(input)?;
        let (input, general_purpose_bit_flag) = be_u16(input)?;
        let (input, compression_method) = be_u16(input)?;
        let (input, last_modification_time) = be_u16(input)?;
        let (input, last_modification_date) = be_u16(input)?;
        let (input, crc_32) = be_u32(input)?;
        let (input, compressed_size) = be_u32(input)?;
        let (input, uncompressed_size) = be_u32(input)?;
        let (input, file_name_length) = be_u16(input)?;
        let (input, extra_field_length) = be_u16(input)?;
        let (input, file_name) = take(file_name_length as usize)(input)?;
        let (input, extra_field) = take(extra_field_length as usize)(input)?;

        Ok((input, ZipLocalFileHeader {
            local_file_header_signature,
            version_needed_to_extract,
            general_purpose_bit_flag,
            compression_method,
            last_modification_time,
            last_modification_date,
            crc_32,
            compressed_size,
            uncompressed_size,
            file_name_length,
            extra_field_length,
            file_name: file_name.to_vec(),
            extra_field: extra_field.to_vec(),
        }))
    }
}

#[derive(Debug)]
struct ZipCentralDirectoryEntry {
    central_directory_signature: u32,
    version_made_by: u16,
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: u16,
    last_modification_time: u16,
    last_modification_date: u16,
    crc_32: u32,
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

impl ZipCentralDirectoryEntry {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, central_directory_signature) = be_u32(input)?;
        let (input, version_made_by) = be_u16(input)?;
        let (input, version_needed_to_extract) = be_u16(input)?;
        let (input, general_purpose_bit_flag) = be_u16(input)?;
        let (input, compression_method) = be_u16(input)?;
        let (input, last_modification_time) = be_u16(input)?;
        let (input, last_modification_date) = be_u16(input)?;
        let (input, crc_32) = be_u32(input)?;
        let (input, compressed_size) = be_u32(input)?;
        let (input, uncompressed_size) = be_u32(input)?;
        let (input, file_name_length) = be_u16(input)?;
        let (input, extra_field_length) = be_u16(input)?;
        let (input, file_comment_length) = be_u16(input)?;
        let (input, disk_number_start) = be_u16(input)?;
        let (input, internal_file_attributes) = be_u16(input)?;
        let (input, external_file_attributes) = be_u32(input)?;
        let (input, local_header_offset) = be_u32(input)?;
        let (input, file_name) = take(file_name_length as usize)(input)?;
        let (input, extra_field) = take(extra_field_length as usize)(input)?;
        let (input, file_comment) = take(file_comment_length as usize)(input)?;

        Ok((input, ZipCentralDirectoryEntry {
            central_directory_signature,
            version_made_by,
            version_needed_to_extract,
            general_purpose_bit_flag,
            compression_method,
            last_modification_time,
            last_modification_date,
            crc_32,
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
        }))
    }
}

#[derive(Debug)]
struct ZipEndOfCentralDirectoryRecord {
    end_of_central_directory_signature: u32,
    disk_number: u16,
    central_directory_disk_number: u16,
    disk_entries: u16,
    total_entries: u16,
    central_directory_size: u32,
    central_directory_offset: u32,
    comment_length: u16,
    comment: Vec<u8>,
}

impl ZipEndOfCentralDirectoryRecord {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, end_of_central_directory_signature) = be_u32(input)?;
        let (input, disk_number) = be_u16(input)?;
        let (input, central_directory_disk_number) = be_u16(input)?;
        let (input, disk_entries) = be_u16(input)?;
        let (input, total_entries) = be_u16(input)?;
        let (input, central_directory_size) = be_u32(input)?;
        let (input, central_directory_offset) = be_u32(input)?;
        let (input, comment_length) = be_u16(input)?;
        let (input, comment) = take(comment_length as usize)(input)?;

        Ok((input, ZipEndOfCentralDirectoryRecord {
            end_of_central_directory_signature,
            disk_number,
            central_directory_disk_number,
            disk_entries,
            total_entries,
            central_directory_size,
            central_directory_offset,
            comment_length,
            comment: comment.to_vec(),
        }))
    }
}

fn main() -> Result<(), Error> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(Error::new(std::io::ErrorKind::InvalidInput, "Usage: zip_parser <zip_file>"));
    }

    let mut file = File::open(&args[1])?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    let (input, local_file_header) = ZipLocalFileHeader::parse(&data);
    println!("Local File Header: {:?}", local_file_header);

    let (input, central_directory_entry) = ZipCentralDirectoryEntry::parse(input);
    println!("Central Directory Entry: {:?}", central_directory_entry);

    let (input, end_of_central_directory_record) = ZipEndOfCentralDirectoryRecord::parse(input);
    println!("End of Central Directory Record: {:?}", end_of_central_directory_record);

    Ok(())
}