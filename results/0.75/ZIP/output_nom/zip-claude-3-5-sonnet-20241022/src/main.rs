use nom::bytes::complete::{tag, take};
use nom::number::complete::{le_u16, le_u32, le_u8};
use nom::sequence::tuple;
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

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
    file_name_length: u16,
    extra_field_length: u16,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
}

#[derive(Debug)]
struct ZipCentralDirectoryHeader {
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
    local_header_offset: u32,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

#[derive(Debug)]
struct ZipEndOfCentralDirectory {
    disk_number: u16,
    start_disk_number: u16,
    total_entries_this_disk: u16,
    total_entries: u16,
    central_directory_size: u32,
    central_directory_offset: u32,
    comment_length: u16,
    comment: Vec<u8>,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], ZipLocalFileHeader> {
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

    Ok((input, ZipLocalFileHeader {
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

fn parse_central_directory_header(input: &[u8]) -> IResult<&[u8], ZipCentralDirectoryHeader> {
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
        local_header_offset
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

    Ok((input, ZipCentralDirectoryHeader {
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
        local_header_offset,
        file_name: file_name.to_vec(),
        extra_field: extra_field.to_vec(),
        file_comment: file_comment.to_vec(),
    }))
}

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], ZipEndOfCentralDirectory> {
    let (input, _) = tag(&[0x50, 0x4b, 0x05, 0x06])(input)?;
    let (input, (
        disk_number,
        start_disk_number,
        total_entries_this_disk,
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

    Ok((input, ZipEndOfCentralDirectory {
        disk_number,
        start_disk_number,
        total_entries_this_disk,
        total_entries,
        central_directory_size,
        central_directory_offset,
        comment_length,
        comment: comment.to_vec(),
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

    let mut current_pos = 0;
    let mut local_headers = Vec::new();
    let mut central_directories = Vec::new();

    // Parse local file headers
    while current_pos + 4 <= buffer.len() {
        if &buffer[current_pos..current_pos + 4] == &[0x50, 0x4b, 0x03, 0x04] {
            match parse_local_file_header(&buffer[current_pos..]) {
                Ok((remaining, header)) => {
                    current_pos = buffer.len() - remaining.len();
                    local_headers.push(header);
                    // Skip file data
                    current_pos += local_headers.last().unwrap().compressed_size as usize;
                }
                Err(_) => break,
            }
        } else {
            current_pos += 1;
        }
    }

    // Parse central directory headers
    current_pos = 0;
    while current_pos + 4 <= buffer.len() {
        if &buffer[current_pos..current_pos + 4] == &[0x50, 0x4b, 0x01, 0x02] {
            match parse_central_directory_header(&buffer[current_pos..]) {
                Ok((remaining, header)) => {
                    current_pos = buffer.len() - remaining.len();
                    central_directories.push(header);
                }
                Err(_) => break,
            }
        } else {
            current_pos += 1;
        }
    }

    // Find and parse end of central directory
    for i in (0..buffer.len()).rev() {
        if i >= 4 && &buffer[i-4..i] == &[0x50, 0x4b, 0x05, 0x06] {
            if let Ok((_, end_of_central_dir)) = parse_end_of_central_directory(&buffer[i-4..]) {
                println!("End of Central Directory: {:?}", end_of_central_dir);
                break;
            }
        }
    }

    println!("Local File Headers: {:?}", local_headers);
    println!("Central Directory Headers: {:?}", central_directories);
}