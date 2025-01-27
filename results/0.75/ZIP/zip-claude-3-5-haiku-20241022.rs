use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::count,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ZipLocalFileHeader {
    version_needed: u16,
    flags: u16,
    compression: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    filename: Vec<u8>,
    extra_field: Option<Vec<u8>>,
}

#[derive(Debug)]
struct ZipCentralDirectoryFileHeader {
    version_made_by: u16,
    version_needed: u16,
    flags: u16,
    compression: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    internal_file_attrs: u16,
    external_file_attrs: u32,
    local_header_offset: u32,
    filename: Vec<u8>,
    extra_field: Option<Vec<u8>>,
    file_comment: Option<Vec<u8>>,
}

#[derive(Debug)]
struct ZipEndOfCentralDirectory {
    disk_number: u16,
    central_dir_disk: u16,
    num_entries_this_disk: u16,
    total_entries: u16,
    central_dir_size: u32,
    central_dir_offset: u32,
    comment_length: u16,
    comment: Option<Vec<u8>>,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], ZipLocalFileHeader> {
    let (input, _) = tag(b"PK\x03\x04")(input)?;
    let (input, (version_needed, flags, compression, last_mod_time, last_mod_date, crc32,
                 compressed_size, uncompressed_size, filename_length, extra_field_length)) = tuple((
        le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16
    ))(input)?;
    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = opt(|i| take(extra_field_length)(i))(input)?;

    Ok((input, ZipLocalFileHeader {
        version_needed,
        flags,
        compression,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_length,
        extra_field_length,
        filename: filename.to_vec(),
        extra_field: extra_field.map(|x| x.to_vec()),
    }))
}

fn parse_central_directory_header(input: &[u8]) -> IResult<&[u8], ZipCentralDirectoryFileHeader> {
    let (input, _) = tag(b"PK\x01\x02")(input)?;
    let (input, (version_made_by, version_needed, flags, compression, last_mod_time, last_mod_date,
                 crc32, compressed_size, uncompressed_size, filename_length, extra_field_length,
                 file_comment_length, disk_number_start, internal_file_attrs, external_file_attrs,
                 local_header_offset)) = tuple((
        le_u16, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16,
        le_u16, le_u16, le_u16, le_u32, le_u32
    ))(input)?;
    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = opt(|i| take(extra_field_length)(i))(input)?;
    let (input, file_comment) = opt(|i| take(file_comment_length)(i))(input)?;

    Ok((input, ZipCentralDirectoryFileHeader {
        version_made_by,
        version_needed,
        flags,
        compression,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_length,
        extra_field_length,
        file_comment_length,
        disk_number_start,
        internal_file_attrs,
        external_file_attrs,
        local_header_offset,
        filename: filename.to_vec(),
        extra_field: extra_field.map(|x| x.to_vec()),
        file_comment: file_comment.map(|x| x.to_vec()),
    }))
}

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], ZipEndOfCentralDirectory> {
    let (input, _) = tag(b"PK\x05\x06")(input)?;
    let (input, (disk_number, central_dir_disk, num_entries_this_disk, total_entries,
                 central_dir_size, central_dir_offset, comment_length)) = tuple((
        le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u16
    ))(input)?;
    let (input, comment) = opt(|i| take(comment_length)(i))(input)?;

    Ok((input, ZipEndOfCentralDirectory {
        disk_number,
        central_dir_disk,
        num_entries_this_disk,
        total_entries,
        central_dir_size,
        central_dir_offset,
        comment_length,
        comment: comment.map(|x| x.to_vec()),
    }))
}

fn parse_zip_file(input: &[u8]) -> Result<(Vec<ZipLocalFileHeader>, Vec<ZipCentralDirectoryFileHeader>, ZipEndOfCentralDirectory), nom::Err<nom::error::Error<&[u8]>>> {
    let mut local_headers = Vec::new();
    let mut central_headers = Vec::new();
    let mut remaining_input = input;

    while !remaining_input.is_empty() {
        match parse_local_file_header(remaining_input) {
            Ok((rest, header)) => {
                local_headers.push(header);
                remaining_input = rest;
            },
            Err(_) => break,
        }
    }

    remaining_input = input;
    while !remaining_input.is_empty() {
        match parse_central_directory_header(remaining_input) {
            Ok((rest, header)) => {
                central_headers.push(header);
                remaining_input = rest;
            },
            Err(_) => break,
        }
    }

    let (_, end_of_central_directory) = parse_end_of_central_directory(remaining_input)?;

    Ok((local_headers, central_headers, end_of_central_directory))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let (local_headers, central_headers, end_of_central_directory) = parse_zip_file(&buffer)?;

    println!("Local File Headers: {:?}", local_headers);
    println!("Central Directory Headers: {:?}", central_headers);
    println!("End of Central Directory: {:?}", end_of_central_directory);

    Ok(())
}