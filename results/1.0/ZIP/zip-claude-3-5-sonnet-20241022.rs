use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u8, le_u16, le_u32, le_u64},
    sequence::tuple,
    IResult,
};

#[derive(Debug)]
struct LocalFileHeader {
    signature: u32,
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
struct CentralDirectoryHeader {
    signature: u32,
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
    comment_length: u16,
    disk_number_start: u16,
    internal_attrs: u16,
    external_attrs: u32,
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
    entries_on_disk: u16,
    total_entries: u16,
    directory_size: u32,
    directory_offset: u32,
    comment_length: u16,
    comment: Vec<u8>,
}

#[derive(Debug)]
struct Zip64EndOfCentralDirectory {
    signature: u32,
    record_size: u64,
    version_made_by: u16,
    version_needed: u16,
    disk_number: u32,
    start_disk_number: u32,
    entries_on_disk: u64,
    total_entries: u64,
    directory_size: u64,
    directory_offset: u64,
    extensible_data: Vec<u8>,
}

#[derive(Debug)]
struct Zip64EndOfCentralDirectoryLocator {
    signature: u32,
    disk_number: u32,
    relative_offset: u64,
    total_disks: u32,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, (
        signature,
        version_needed,
        flags,
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_length,
        extra_field_length
    )) = tuple((
        le_u32,
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

    Ok((input, LocalFileHeader {
        signature,
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

fn parse_central_directory_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryHeader> {
    let (input, (
        signature,
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
        comment_length,
        disk_number_start,
        internal_attrs,
        external_attrs,
        local_header_offset
    )) = tuple((
        le_u32,
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
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_length,
        extra_field_length,
        comment_length,
        disk_number_start,
        internal_attrs,
        external_attrs,
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
        entries_on_disk,
        total_entries,
        directory_size,
        directory_offset,
        comment_length
    )) = tuple((
        le_u32,
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
        entries_on_disk,
        total_entries,
        directory_size,
        directory_offset,
        comment_length,
        comment: comment.to_vec(),
    }))
}

fn parse_zip64_end_of_central_directory(input: &[u8]) -> IResult<&[u8], Zip64EndOfCentralDirectory> {
    let (input, (
        signature,
        record_size,
        version_made_by,
        version_needed,
        disk_number,
        start_disk_number,
        entries_on_disk,
        total_entries,
        directory_size,
        directory_offset
    )) = tuple((
        le_u32,
        le_u64,
        le_u16,
        le_u16,
        le_u32,
        le_u32,
        le_u64,
        le_u64,
        le_u64,
        le_u64
    ))(input)?;

    let (input, extensible_data) = take(record_size - 44)(input)?;

    Ok((input, Zip64EndOfCentralDirectory {
        signature,
        record_size,
        version_made_by,
        version_needed,
        disk_number,
        start_disk_number,
        entries_on_disk,
        total_entries,
        directory_size,
        directory_offset,
        extensible_data: extensible_data.to_vec(),
    }))
}

fn parse_zip64_end_of_central_directory_locator(input: &[u8]) -> IResult<&[u8], Zip64EndOfCentralDirectoryLocator> {
    let (input, (
        signature,
        disk_number,
        relative_offset,
        total_disks
    )) = tuple((
        le_u32,
        le_u32,
        le_u64,
        le_u32
    ))(input)?;

    Ok((input, Zip64EndOfCentralDirectoryLocator {
        signature,
        disk_number,
        relative_offset,
        total_disks,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <zip_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let mut input = &buffer[..];
    while !input.is_empty() {
        if input.len() >= 4 {
            match &input[0..4] {
                [0x50, 0x4b, 0x03, 0x04] => {
                    match parse_local_file_header(input) {
                        Ok((remaining, header)) => {
                            println!("Local File Header: {:?}", header);
                            input = remaining;
                            input = &input[header.compressed_size as usize..];
                        },
                        Err(e) => {
                            println!("Error parsing local file header: {:?}", e);
                            break;
                        }
                    }
                },
                [0x50, 0x4b, 0x01, 0x02] => {
                    match parse_central_directory_header(input) {
                        Ok((remaining, header)) => {
                            println!("Central Directory Header: {:?}", header);
                            input = remaining;
                        },
                        Err(e) => {
                            println!("Error parsing central directory header: {:?}", e);
                            break;
                        }
                    }
                },
                [0x50, 0x4b, 0x06, 0x06] => {
                    match parse_zip64_end_of_central_directory(input) {
                        Ok((remaining, header)) => {
                            println!("ZIP64 End of Central Directory: {:?}", header);
                            input = remaining;
                        },
                        Err(e) => {
                            println!("Error parsing ZIP64 end of central directory: {:?}", e);
                            break;
                        }
                    }
                },
                [0x50, 0x4b, 0x06, 0x07] => {
                    match parse_zip64_end_of_central_directory_locator(input) {
                        Ok((remaining, header)) => {
                            println!("ZIP64 End of Central Directory Locator: {:?}", header);
                            input = remaining;
                        },
                        Err(e) => {
                            println!("Error parsing ZIP64 end of central directory locator: {:?}", e);
                            break;
                        }
                    }
                },
                [0x50, 0x4b, 0x05, 0x06] => {
                    match parse_end_of_central_directory(input) {
                        Ok((remaining, header)) => {
                            println!("End of Central Directory: {:?}", header);
                            input = remaining;
                        },
                        Err(e) => {
                            println!("Error parsing end of central directory: {:?}", e);
                            break;
                        }
                    }
                },
                _ => {
                    input = &input[1..];
                }
            }
        } else {
            break;
        }
    }
}