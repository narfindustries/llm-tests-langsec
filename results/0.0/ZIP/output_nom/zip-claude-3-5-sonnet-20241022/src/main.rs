use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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
    file_name_length: u16,
    extra_field_length: u16,
    file_name: Vec<u8>,
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
    file_name_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    internal_attrs: u16,
    external_attrs: u32,
    local_header_offset: u32,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

#[derive(Debug)]
struct EndOfCentralDirectory {
    signature: u32,
    disk_number: u16,
    start_disk_number: u16,
    total_entries_disk: u16,
    total_entries: u16,
    central_dir_size: u32,
    central_dir_offset: u32,
    comment_length: u16,
    comment: Vec<u8>,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, signature) = le_u32(input)?;
    let (input, version_needed) = le_u16(input)?;
    let (input, flags) = le_u16(input)?;
    let (input, compression_method) = le_u16(input)?;
    let (input, last_mod_time) = le_u16(input)?;
    let (input, last_mod_date) = le_u16(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, compressed_size) = le_u32(input)?;
    let (input, uncompressed_size) = le_u32(input)?;
    let (input, file_name_length) = le_u16(input)?;
    let (input, extra_field_length) = le_u16(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;

    Ok((
        input,
        LocalFileHeader {
            signature,
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
        },
    ))
}

fn parse_central_directory_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryHeader> {
    let (input, signature) = le_u32(input)?;
    let (input, version_made_by) = le_u16(input)?;
    let (input, version_needed) = le_u16(input)?;
    let (input, flags) = le_u16(input)?;
    let (input, compression_method) = le_u16(input)?;
    let (input, last_mod_time) = le_u16(input)?;
    let (input, last_mod_date) = le_u16(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, compressed_size) = le_u32(input)?;
    let (input, uncompressed_size) = le_u32(input)?;
    let (input, file_name_length) = le_u16(input)?;
    let (input, extra_field_length) = le_u16(input)?;
    let (input, file_comment_length) = le_u16(input)?;
    let (input, disk_number_start) = le_u16(input)?;
    let (input, internal_attrs) = le_u16(input)?;
    let (input, external_attrs) = le_u32(input)?;
    let (input, local_header_offset) = le_u32(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    Ok((
        input,
        CentralDirectoryHeader {
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
            file_name_length,
            extra_field_length,
            file_comment_length,
            disk_number_start,
            internal_attrs,
            external_attrs,
            local_header_offset,
            file_name: file_name.to_vec(),
            extra_field: extra_field.to_vec(),
            file_comment: file_comment.to_vec(),
        },
    ))
}

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectory> {
    let (input, signature) = le_u32(input)?;
    let (input, disk_number) = le_u16(input)?;
    let (input, start_disk_number) = le_u16(input)?;
    let (input, total_entries_disk) = le_u16(input)?;
    let (input, total_entries) = le_u16(input)?;
    let (input, central_dir_size) = le_u32(input)?;
    let (input, central_dir_offset) = le_u32(input)?;
    let (input, comment_length) = le_u16(input)?;
    let (input, comment) = take(comment_length)(input)?;

    Ok((
        input,
        EndOfCentralDirectory {
            signature,
            disk_number,
            start_disk_number,
            total_entries_disk,
            total_entries,
            central_dir_size,
            central_dir_offset,
            comment_length,
            comment: comment.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut contents = Vec::new();
    file.read_to_end(&mut contents).expect("Failed to read file");

    let mut input = contents.as_slice();
    while input.len() > 0 {
        if input.len() >= 4 {
            match le_u32::<&[u8], nom::error::Error<&[u8]>>(&input[..4]) {
                Ok((_, 0x04034b50)) => {
                    match parse_local_file_header(input) {
                        Ok((remaining, header)) => {
                            println!("Local File Header: {:?}", header);
                            input = remaining;
                        }
                        Err(e) => {
                            eprintln!("Error parsing local file header: {:?}", e);
                            break;
                        }
                    }
                }
                Ok((_, 0x02014b50)) => {
                    match parse_central_directory_header(input) {
                        Ok((remaining, header)) => {
                            println!("Central Directory Header: {:?}", header);
                            input = remaining;
                        }
                        Err(e) => {
                            eprintln!("Error parsing central directory header: {:?}", e);
                            break;
                        }
                    }
                }
                Ok((_, 0x06054b50)) => {
                    match parse_end_of_central_directory(input) {
                        Ok((remaining, header)) => {
                            println!("End of Central Directory: {:?}", header);
                            input = remaining;
                        }
                        Err(e) => {
                            eprintln!("Error parsing end of central directory: {:?}", e);
                            break;
                        }
                    }
                }
                _ => {
                    input = &input[1..];
                }
            }
        } else {
            break;
        }
    }
}