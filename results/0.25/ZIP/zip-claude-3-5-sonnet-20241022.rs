use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::many0,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct LocalFileHeader {
    version_needed: u16,
    flags: u16,
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
struct CentralDirectoryHeader {
    version_made_by: u16,
    version_needed: u16,
    flags: u16,
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
struct EndOfCentralDirectory {
    disk_number: u16,
    start_disk_number: u16,
    total_entries_disk: u16,
    total_entries: u16,
    central_directory_size: u32,
    central_directory_offset: u32,
    comment_length: u16,
    comment: Vec<u8>,
}

#[derive(Debug)]
struct ZipFile {
    local_headers: Vec<LocalFileHeader>,
    central_directory: Vec<CentralDirectoryHeader>,
    end_of_central_directory: EndOfCentralDirectory,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, _) = tag(&[0x50, 0x4b, 0x03, 0x04])(input)?;
    let (input, (
        version_needed,
        flags,
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        file_name_length,
        extra_field_length
    )) = tuple((
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u32,
        le_u32,
        le_u32,
        le_u16,
        le_u16
    ))(input)?;

    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;

    Ok((input, LocalFileHeader {
        version_needed,
        flags,
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

fn parse_central_directory_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryHeader> {
    let (input, _) = tag(&[0x50, 0x4b, 0x01, 0x02])(input)?;
    let (input, (
        version_made_by,
        version_needed,
        flags,
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
        relative_offset
    )) = tuple((
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u32,
        le_u32,
        le_u32,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u32,
        le_u32
    ))(input)?;

    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    Ok((input, CentralDirectoryHeader {
        version_made_by,
        version_needed,
        flags,
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
    }))
}

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectory> {
    let (input, _) = tag(&[0x50, 0x4b, 0x05, 0x06])(input)?;
    let (input, (
        disk_number,
        start_disk_number,
        total_entries_disk,
        total_entries,
        central_directory_size,
        central_directory_offset,
        comment_length
    )) = tuple((
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u32,
        le_u32,
        le_u16
    ))(input)?;

    let (input, comment) = take(comment_length)(input)?;

    Ok((input, EndOfCentralDirectory {
        disk_number,
        start_disk_number,
        total_entries_disk,
        total_entries,
        central_directory_size,
        central_directory_offset,
        comment_length,
        comment: comment.to_vec(),
    }))
}

fn parse_zip_file(input: &[u8]) -> IResult<&[u8], ZipFile> {
    let (input, local_headers) = many0(parse_local_file_header)(input)?;
    let (input, central_directory) = many0(parse_central_directory_header)(input)?;
    let (input, end_of_central_directory) = parse_end_of_central_directory(input)?;

    Ok((input, ZipFile {
        local_headers,
        central_directory,
        end_of_central_directory,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_zip_file(&buffer) {
        Ok((_, zip_file)) => println!("{:#?}", zip_file),
        Err(e) => eprintln!("Failed to parse ZIP file: {:?}", e),
    }
}