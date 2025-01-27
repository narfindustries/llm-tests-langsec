use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{many0, many1},
    number::complete::{be_u16, be_u32, be_u64},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
struct ZipLocalFileHeader {
    local_file_header_signature: u32,
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
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
        let (input, last_mod_time) = be_u16(input)?;
        let (input, last_mod_date) = be_u16(input)?;
        let (input, crc_32) = be_u32(input)?;
        let (input, compressed_size) = be_u32(input)?;
        let (input, uncompressed_size) = be_u32(input)?;
        let (input, file_name_length) = be_u16(input)?;
        let (input, extra_field_length) = be_u16(input)?;
        let (input, file_name) = take(file_name_length)(input)?;
        let (input, extra_field) = take(extra_field_length)(input)?;
        Ok((
            input,
            ZipLocalFileHeader {
                local_file_header_signature,
                version_needed_to_extract,
                general_purpose_bit_flag,
                compression_method,
                last_mod_time,
                last_mod_date,
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
}

#[derive(Debug)]
struct ZipCentralDirectoryEntry {
    central_directory_entry_signature: u32,
    version_made_by: u16,
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
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

impl ZipCentralDirectoryEntry {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, central_directory_entry_signature) = be_u32(input)?;
        let (input, version_made_by) = be_u16(input)?;
        let (input, version_needed_to_extract) = be_u16(input)?;
        let (input, general_purpose_bit_flag) = be_u16(input)?;
        let (input, compression_method) = be_u16(input)?;
        let (input, last_mod_time) = be_u16(input)?;
        let (input, last_mod_date) = be_u16(input)?;
        let (input, crc_32) = be_u32(input)?;
        let (input, compressed_size) = be_u32(input)?;
        let (input, uncompressed_size) = be_u32(input)?;
        let (input, file_name_length) = be_u16(input)?;
        let (input, extra_field_length) = be_u16(input)?;
        let (input, file_comment_length) = be_u16(input)?;
        let (input, disk_number_start) = be_u16(input)?;
        let (input, internal_file_attributes) = be_u16(input)?;
        let (input, external_file_attributes) = be_u32(input)?;
        let (input, relative_offset_of_local_header) = be_u32(input)?;
        let (input, file_name) = take(file_name_length)(input)?;
        let (input, extra_field) = take(extra_field_length)(input)?;
        let (input, file_comment) = take(file_comment_length)(input)?;
        Ok((
            input,
            ZipCentralDirectoryEntry {
                central_directory_entry_signature,
                version_made_by,
                version_needed_to_extract,
                general_purpose_bit_flag,
                compression_method,
                last_mod_time,
                last_mod_date,
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
}

#[derive(Debug)]
struct ZipEndOfCentralDirectory {
    end_of_central_directory_signature: u32,
    number_of_this_disk: u16,
    number_of_the_disk_where_the_central_directory_starts: u16,
    total_number_of_entries_in_the_central_directory_on_this_disk: u16,
    total_number_of_entries_in_the_central_directory: u16,
    size_of_the_central_directory: u32,
    offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number: u32,
    zip_file_comment_length: u16,
    zip_file_comment: Vec<u8>,
}

impl ZipEndOfCentralDirectory {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, end_of_central_directory_signature) = be_u32(input)?;
        let (input, number_of_this_disk) = be_u16(input)?;
        let (input, number_of_the_disk_where_the_central_directory_starts) = be_u16(input)?;
        let (input, total_number_of_entries_in_the_central_directory_on_this_disk) = be_u16(input)?;
        let (input, total_number_of_entries_in_the_central_directory) = be_u16(input)?;
        let (input, size_of_the_central_directory) = be_u32(input)?;
        let (input, offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number) =
            be_u32(input)?;
        let (input, zip_file_comment_length) = be_u16(input)?;
        let (input, zip_file_comment) = take(zip_file_comment_length)(input)?;
        Ok((
            input,
            ZipEndOfCentralDirectory {
                end_of_central_directory_signature,
                number_of_this_disk,
                number_of_the_disk_where_the_central_directory_starts,
                total_number_of_entries_in_the_central_directory_on_this_disk,
                total_number_of_entries_in_the_central_directory,
                size_of_the_central_directory,
                offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number,
                zip_file_comment_length,
                zip_file_comment: zip_file_comment.to_vec(),
            },
        ))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <zip_file>", args[0]);
        return;
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut data = Vec::new();
    reader.read_to_end(&mut data).unwrap();
    let mut offset = 0;
    loop {
        let (input, local_file_header) = ZipLocalFileHeader::parse(&data[offset..]);
        if input.is_empty() {
            break;
        }
        offset += input.len();
        println!("{:?}", local_file_header);
        offset += local_file_header.file_name_length as usize + local_file_header.extra_field_length as usize;
        offset += local_file_header.compressed_size as usize;
    }
    let (input, central_directory_entries) = many1(ZipCentralDirectoryEntry::parse)(&data);
    for entry in central_directory_entries {
        println!("{:?}", entry);
    }
    let (input, end_of_central_directory) = ZipEndOfCentralDirectory::parse(input);
    println!("{:?}", end_of_central_directory);
}