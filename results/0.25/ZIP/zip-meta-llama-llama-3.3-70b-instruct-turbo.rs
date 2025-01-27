use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{many0, many1},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, stdin};
use std::path::Path;

#[derive(Debug)]
struct LocalFileHeader {
    header_signature: u32,
    version: u16,
    flags: u16,
    compression_method: u16,
    last_modification_time: u16,
    last_modification_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    filename: String,
    extra_field: Vec<u8>,
}

impl LocalFileHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, header_signature) = be_u32(input)?;
        let (input, version) = be_u16(input)?;
        let (input, flags) = be_u16(input)?;
        let (input, compression_method) = be_u16(input)?;
        let (input, last_modification_time) = be_u16(input)?;
        let (input, last_modification_date) = be_u16(input)?;
        let (input, crc32) = be_u32(input)?;
        let (input, compressed_size) = be_u32(input)?;
        let (input, uncompressed_size) = be_u32(input)?;
        let (input, filename_length) = be_u16(input)?;
        let (input, extra_field_length) = be_u16(input)?;
        let (input, filename) = take(filename_length)(input)?;
        let (input, extra_field) = take(extra_field_length)(input)?;
        let filename = String::from_utf8_lossy(filename).into_owned();
        Ok((input, Self {
            header_signature,
            version,
            flags,
            compression_method,
            last_modification_time,
            last_modification_date,
            crc32,
            compressed_size,
            uncompressed_size,
            filename_length,
            extra_field_length,
            filename,
            extra_field: extra_field.to_vec(),
        }))
    }
}

#[derive(Debug)]
struct CentralDirectoryEntry {
    header_signature: u32,
    version_made_by: u16,
    version_to_extract: u16,
    flags: u16,
    compression_method: u16,
    last_modification_time: u16,
    last_modification_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    internal_file_attributes: u16,
    external_file_attributes: u32,
    local_header_offset: u32,
    filename: String,
    extra_field: Vec<u8>,
    file_comment: String,
}

impl CentralDirectoryEntry {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, header_signature) = be_u32(input)?;
        let (input, version_made_by) = be_u16(input)?;
        let (input, version_to_extract) = be_u16(input)?;
        let (input, flags) = be_u16(input)?;
        let (input, compression_method) = be_u16(input)?;
        let (input, last_modification_time) = be_u16(input)?;
        let (input, last_modification_date) = be_u16(input)?;
        let (input, crc32) = be_u32(input)?;
        let (input, compressed_size) = be_u32(input)?;
        let (input, uncompressed_size) = be_u32(input)?;
        let (input, filename_length) = be_u16(input)?;
        let (input, extra_field_length) = be_u16(input)?;
        let (input, file_comment_length) = be_u16(input)?;
        let (input, disk_number_start) = be_u16(input)?;
        let (input, internal_file_attributes) = be_u16(input)?;
        let (input, external_file_attributes) = be_u32(input)?;
        let (input, local_header_offset) = be_u32(input)?;
        let (input, filename) = take(filename_length)(input)?;
        let (input, extra_field) = take(extra_field_length)(input)?;
        let (input, file_comment) = take(file_comment_length)(input)?;
        let filename = String::from_utf8_lossy(filename).into_owned();
        let file_comment = String::from_utf8_lossy(file_comment).into_owned();
        Ok((input, Self {
            header_signature,
            version_made_by,
            version_to_extract,
            flags,
            compression_method,
            last_modification_time,
            last_modification_date,
            crc32,
            compressed_size,
            uncompressed_size,
            filename_length,
            extra_field_length,
            file_comment_length,
            disk_number_start,
            internal_file_attributes,
            external_file_attributes,
            local_header_offset,
            filename,
            extra_field: extra_field.to_vec(),
            file_comment,
        }))
    }
}

#[derive(Debug)]
struct EndOfCentralDirectory {
    header_signature: u32,
    num_this_disk: u16,
    num_disk_with_cd: u16,
    num_entries_this_disk: u16,
    num_entries_total: u16,
    cd_size: u32,
    cd_offset: u32,
    comment_length: u16,
    comment: String,
}

impl EndOfCentralDirectory {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, header_signature) = be_u32(input)?;
        let (input, num_this_disk) = be_u16(input)?;
        let (input, num_disk_with_cd) = be_u16(input)?;
        let (input, num_entries_this_disk) = be_u16(input)?;
        let (input, num_entries_total) = be_u16(input)?;
        let (input, cd_size) = be_u32(input)?;
        let (input, cd_offset) = be_u32(input)?;
        let (input, comment_length) = be_u16(input)?;
        let (input, comment) = take(comment_length)(input)?;
        let comment = String::from_utf8_lossy(comment).into_owned();
        Ok((input, Self {
            header_signature,
            num_this_disk,
            num_disk_with_cd,
            num_entries_this_disk,
            num_entries_total,
            cd_size,
            cd_offset,
            comment_length,
            comment,
        }))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        return;
    }
    let path = Path::new(&args[1]);
    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();
    let mut pos = 0;
    let mut local_file_headers = Vec::new();
    while pos < data.len() {
        let (input, local_file_header) = LocalFileHeader::parse(&data[pos..]).unwrap();
        pos += input.len();
        local_file_headers.push(local_file_header);
        let (input, _) = take(local_file_header.compressed_size)(input).unwrap();
        pos += input.len();
    }
    let (input, _) = take(data.len() - pos - 22)(data.as_slice()).unwrap();
    let (input, end_of_central_directory) = EndOfCentralDirectory::parse(input).unwrap();
    let mut central_directory_entries = Vec::new();
    for i in 0..end_of_central_directory.num_entries_total {
        let (input, central_directory_entry) = CentralDirectoryEntry::parse(input).unwrap();
        central_directory_entries.push(central_directory_entry);
    }
    println!("{:?}", local_file_headers);
    println!("{:?}", central_directory_entries);
    println!("{:?}", end_of_central_directory);
}