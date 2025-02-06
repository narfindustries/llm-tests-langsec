use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::{env, fs};

#[derive(Debug)]
enum CompressionMethod {
    Stored,
    Shrunk,
    Imploded,
    Tokenized,
    Deflated,
    Imploded2,
    Tokenized2,
    Deflated64,
    Bzip2,
    Lzma,
    Terse,
    Lz77,
    Wavpack,
    Ppmd,
    Aes,
    Zstandard,
}

impl CompressionMethod {
    fn from_u16(value: u16) -> CompressionMethod {
        match value {
            0 => CompressionMethod::Stored,
            1 => CompressionMethod::Shrunk,
            2 => CompressionMethod::Imploded,
            3 => CompressionMethod::Tokenized,
            4 => CompressionMethod::Deflated,
            5 => CompressionMethod::Imploded2,
            6 => CompressionMethod::Tokenized2,
            7 => CompressionMethod::Deflated64,
            8 => CompressionMethod::Bzip2,
            9 => CompressionMethod::Lzma,
            10 => CompressionMethod::Terse,
            11 => CompressionMethod::Lz77,
            12 => CompressionMethod::Wavpack,
            13 => CompressionMethod::Ppmd,
            14 => CompressionMethod::Aes,
            93 => CompressionMethod::Zstandard,
            _ => panic!("Invalid compression method"),
        }
    }
}

#[derive(Debug)]
struct LocalHeader {
    signature: u32,
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: CompressionMethod,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    filename: Vec<u8>,
    extra_field: Vec<u8>,
}

impl LocalHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], LocalHeader> {
        let (input, signature) = map(tag(&[0x50, 0x4b, 0x03, 0x04]), |x: &[u8]| u32::from_be_bytes([x[0], x[1], x[2], x[3]]))(input)?;
        let (input, version_needed_to_extract) = be_u16(input)?;
        let (input, general_purpose_bit_flag) = be_u16(input)?;
        let (input, compression_method) = map(be_u16, CompressionMethod::from_u16)(input)?;
        let (input, last_mod_time) = be_u16(input)?;
        let (input, last_mod_date) = be_u16(input)?;
        let (input, crc32) = be_u32(input)?;
        let (input, compressed_size) = be_u32(input)?;
        let (input, uncompressed_size) = be_u32(input)?;
        let (input, filename_length) = be_u16(input)?;
        let (input, extra_field_length) = be_u16(input)?;
        let (input, filename) = take(filename_length)(input)?;
        let (input, extra_field) = take(extra_field_length)(input)?;
        Ok((
            input,
            LocalHeader {
                signature,
                version_needed_to_extract,
                general_purpose_bit_flag,
                compression_method,
                last_mod_time,
                last_mod_date,
                crc32,
                compressed_size,
                uncompressed_size,
                filename_length,
                extra_field_length,
                filename: filename.to_vec(),
                extra_field: extra_field.to_vec(),
            },
        ))
    }
}

#[derive(Debug)]
struct CentralDirectoryHeader {
    signature: u32,
    version_made_by: u16,
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: CompressionMethod,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    int_file_attributes: u16,
    ext_file_attributes: u32,
    filename: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

impl CentralDirectoryHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], CentralDirectoryHeader> {
        let (input, signature) = map(tag(&[0x50, 0x4b, 0x01, 0x02]), |x: &[u8]| u32::from_be_bytes([x[0], x[1], x[2], x[3]]))(input)?;
        let (input, version_made_by) = be_u16(input)?;
        let (input, version_needed_to_extract) = be_u16(input)?;
        let (input, general_purpose_bit_flag) = be_u16(input)?;
        let (input, compression_method) = map(be_u16, CompressionMethod::from_u16)(input)?;
        let (input, last_mod_time) = be_u16(input)?;
        let (input, last_mod_date) = be_u16(input)?;
        let (input, crc32) = be_u32(input)?;
        let (input, compressed_size) = be_u32(input)?;
        let (input, uncompressed_size) = be_u32(input)?;
        let (input, filename_length) = be_u16(input)?;
        let (input, extra_field_length) = be_u16(input)?;
        let (input, file_comment_length) = be_u16(input)?;
        let (input, disk_number_start) = be_u16(input)?;
        let (input, int_file_attributes) = be_u16(input)?;
        let (input, ext_file_attributes) = be_u32(input)?;
        let (input, filename) = take(filename_length)(input)?;
        let (input, extra_field) = take(extra_field_length)(input)?;
        let (input, file_comment) = take(file_comment_length)(input)?;
        Ok((
            input,
            CentralDirectoryHeader {
                signature,
                version_made_by,
                version_needed_to_extract,
                general_purpose_bit_flag,
                compression_method,
                last_mod_time,
                last_mod_date,
                crc32,
                compressed_size,
                uncompressed_size,
                filename_length,
                extra_field_length,
                file_comment_length,
                disk_number_start,
                int_file_attributes,
                ext_file_attributes,
                filename: filename.to_vec(),
                extra_field: extra_field.to_vec(),
                file_comment: file_comment.to_vec(),
            },
        ))
    }
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    signature: u32,
    number_of_this_disk: u16,
    number_of_the_disk_where_the_central_directory_starts: u16,
    total_number_of_entries_in_the_central_directory_on_this_disk: u16,
    total_number_of_entries_in_the_central_directory: u16,
    size_of_the_central_directory: u32,
    offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number: u32,
    zipfile_comment_length: u16,
    zipfile_comment: Vec<u8>,
}

impl EndOfCentralDirectoryRecord {
    fn parse(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
        let (input, signature) = map(tag(&[0x50, 0x4b, 0x05, 0x06]), |x: &[u8]| u32::from_be_bytes([x[0], x[1], x[2], x[3]]))(input)?;
        let (input, number_of_this_disk) = be_u16(input)?;
        let (input, number_of_the_disk_where_the_central_directory_starts) = be_u16(input)?;
        let (input, total_number_of_entries_in_the_central_directory_on_this_disk) = be_u16(input)?;
        let (input, total_number_of_entries_in_the_central_directory) = be_u16(input)?;
        let (input, size_of_the_central_directory) = be_u32(input)?;
        let (input, offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number) =
            be_u32(input)?;
        let (input, zipfile_comment_length) = be_u16(input)?;
        let (input, zipfile_comment) = take(zipfile_comment_length)(input)?;
        Ok((
            input,
            EndOfCentralDirectoryRecord {
                signature,
                number_of_this_disk,
                number_of_the_disk_where_the_central_directory_starts,
                total_number_of_entries_in_the_central_directory_on_this_disk,
                total_number_of_entries_in_the_central_directory,
                size_of_the_central_directory,
                offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number,
                zipfile_comment_length,
                zipfile_comment: zipfile_comment.to_vec(),
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
    let file_name = &args[1];
    let file = fs::read(file_name).expect("Failed to read file");
    let mut offset = 0;
    while offset < file.len() {
        let (remaining, local_header) = LocalHeader::parse(&file[offset..]).unwrap();
        offset += remaining.len();
        println!("Local Header: {:?}", local_header);
        offset += local_header.compressed_size as usize;
        if offset >= file.len() {
            break;
        }
        let (remaining, central_directory_header) =
            CentralDirectoryHeader::parse(&file[offset..]).unwrap();
        offset += remaining.len();
        println!("Central Directory Header: {:?}", central_directory_header);
    }
    let (_, end_of_central_directory_record) =
        EndOfCentralDirectoryRecord::parse(&file[file.len() - 22..]).unwrap();
    println!("End of Central Directory Record: {:?}", end_of_central_directory_record);
}