use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::many0,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::{env, fs, io::Read};

#[derive(Debug)]
struct ZipEntry {
    signature: u32,
    version: u16,
    flags: u16,
    compression: u16,
    mod_time: u16,
    mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    filename: Vec<u8>,
    extra_field: Vec<u8>,
    data: Vec<u8>,
}

#[derive(Debug)]
struct CentralDirectoryHeader {
    signature: u32,
    version_made_by: u16,
    version_needed: u16,
    flags: u16,
    compression: u16,
    mod_time: u16,
    mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    comment_length: u16,
    disk_number_start: u16,
    internal_attributes: u16,
    external_attributes: u32,
    local_header_offset: u32,
    filename: Vec<u8>,
    extra_field: Vec<u8>,
    comment: Vec<u8>,
}

#[derive(Debug)]
struct EndOfCentralDirectory {
    signature: u32,
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
    entries: Vec<ZipEntry>,
    central_directory: Vec<CentralDirectoryHeader>,
    end_of_central_directory: EndOfCentralDirectory,
}

fn parse_zip_entry(input: &[u8]) -> IResult<&[u8], ZipEntry> {
    let (input, (
        signature,
        version,
        flags,
        compression,
        mod_time,
        mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_length,
        extra_field_length
    )) = tuple((
        verify(le_u32, |&x| x == 0x04034b50),
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

    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, data) = take(compressed_size)(input)?;

    Ok((input, ZipEntry {
        signature,
        version,
        flags,
        compression,
        mod_time,
        mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_length,
        extra_field_length,
        filename: filename.to_vec(),
        extra_field: extra_field.to_vec(),
        data: data.to_vec(),
    }))
}

fn parse_central_directory_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryHeader> {
    let (input, (
        signature,
        version_made_by,
        version_needed,
        flags,
        compression,
        mod_time,
        mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_length,
        extra_field_length,
        comment_length,
        disk_number_start,
        internal_attributes,
        external_attributes,
        local_header_offset
    )) = tuple((
        verify(le_u32, |&x| x == 0x02014b50),
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

    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, comment) = take(comment_length)(input)?;

    Ok((input, CentralDirectoryHeader {
        signature,
        version_made_by,
        version_needed,
        flags,
        compression,
        mod_time,
        mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_length,
        extra_field_length,
        comment_length,
        disk_number_start,
        internal_attributes,
        external_attributes,
        local_header_offset,
        filename: filename.to_vec(),
        extra_field: extra_field.to_vec(),
        comment: comment.to_vec(),
    }))
}

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectory> {
    let (input, (
        signature,
        disk_number,
        start_disk_number,
        total_entries_disk,
        total_entries,
        central_directory_size,
        central_directory_offset,
        comment_length
    )) = tuple((
        verify(le_u32, |&x| x == 0x06054b50),
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
        signature,
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
    let (input, entries) = many0(parse_zip_entry)(input)?;
    let (input, central_directory) = many0(parse_central_directory_header)(input)?;
    let (input, end_of_central_directory) = parse_end_of_central_directory(input)?;

    Ok((input, ZipFile {
        entries,
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

    let mut file = fs::File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_zip_file(&buffer) {
        Ok((_, zip_file)) => println!("{:#?}", zip_file),
        Err(e) => eprintln!("Failed to parse ZIP file: {:?}", e),
    }
}