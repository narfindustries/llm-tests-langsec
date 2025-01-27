use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::count,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;
use std::io;

#[derive(Debug)]
struct ZipLocalFileHeader {
    version_needed: u16,
    flags: u16,
    compression_method: u16,
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

#[derive(Debug)]
struct ZipCentralDirectoryFileHeader {
    version_made_by: u16,
    version_needed: u16,
    flags: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
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
    filename: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

#[derive(Debug)]
struct ZipEndOfCentralDirectory {
    number_of_this_disk: u16,
    disk_with_central_directory: u16,
    total_entries_this_disk: u16,
    total_entries: u16,
    central_directory_size: u32,
    central_directory_offset: u32,
    comment_length: u16,
    comment: Vec<u8>,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], ZipLocalFileHeader> {
    let (input, _) = tag(b"PK\x03\x04")(input)?;
    let (input, (version_needed, flags, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, filename_length, extra_field_length)) = tuple((
        le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16
    ))(input)?;
    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;

    Ok((input, ZipLocalFileHeader {
        version_needed,
        flags,
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
    }))
}

fn parse_central_directory_file_header(input: &[u8]) -> IResult<&[u8], ZipCentralDirectoryFileHeader> {
    let (input, _) = tag(b"PK\x01\x02")(input)?;
    let (input, (version_made_by, version_needed, flags, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, filename_length, extra_field_length, file_comment_length, disk_number_start, internal_file_attributes, external_file_attributes, local_header_offset)) = tuple((
        le_u16, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32
    ))(input)?;
    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    Ok((input, ZipCentralDirectoryFileHeader {
        version_made_by,
        version_needed,
        flags,
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
        internal_file_attributes,
        external_file_attributes,
        local_header_offset,
        filename: filename.to_vec(),
        extra_field: extra_field.to_vec(),
        file_comment: file_comment.to_vec(),
    }))
}

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], ZipEndOfCentralDirectory> {
    let (input, _) = tag(b"PK\x05\x06")(input)?;
    let (input, (number_of_this_disk, disk_with_central_directory, total_entries_this_disk, total_entries, central_directory_size, central_directory_offset, comment_length)) = tuple((
        le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u16
    ))(input)?;
    let (input, comment) = take(comment_length)(input)?;

    Ok((input, ZipEndOfCentralDirectory {
        number_of_this_disk,
        disk_with_central_directory,
        total_entries_this_disk,
        total_entries,
        central_directory_size,
        central_directory_offset,
        comment_length,
        comment: comment.to_vec(),
    }))
}

fn parse_zip_file(input: &[u8]) -> IResult<&[u8], (Vec<ZipLocalFileHeader>, Vec<ZipCentralDirectoryFileHeader>, ZipEndOfCentralDirectory)> {
    let (input, local_file_headers) = many0(parse_local_file_header)(input)?;
    let (input, central_directory_headers) = many0(parse_central_directory_file_header)(input)?;
    let (input, end_of_central_directory) = parse_end_of_central_directory(input)?;

    Ok((input, (local_file_headers, central_directory_headers, end_of_central_directory)))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        std::process::exit(1);
    }

    let zip_data = fs::read(&args[1])?;
    match parse_zip_file(&zip_data) {
        Ok((_, result)) => {
            println!("Parsed ZIP file: {:?}", result);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse ZIP file: {:?}", e);
            std::process::exit(1);
        }
    }
}