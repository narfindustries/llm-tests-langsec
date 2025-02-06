use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    sequence::tuple,
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
    central_dir_disk: u16,
    disk_entries: u16,
    total_entries: u16,
    central_dir_size: u32,
    central_dir_offset: u32,
    comment_length: u16,
    comment: Vec<u8>,
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
        file_name_length,
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

    let (input, file_name) = take(file_name_length)(input)?;
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
        file_name_length,
        extra_field_length,
        file_name: file_name.to_vec(),
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
        file_name_length,
        extra_field_length,
        file_comment_length,
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

    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

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
    }))
}

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectory> {
    let (input, (
        signature,
        disk_number,
        central_dir_disk,
        disk_entries,
        total_entries,
        central_dir_size,
        central_dir_offset,
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
        central_dir_disk,
        disk_entries,
        total_entries,
        central_dir_size,
        central_dir_offset,
        comment_length,
        comment: comment.to_vec(),
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip-file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let mut input = &buffer[..];
    
    // Parse local file headers
    while let Ok((remaining, header)) = parse_local_file_header(input) {
        if header.signature != 0x04034b50 {
            break;
        }
        println!("Local File Header: {:?}", header);
        input = &remaining[header.compressed_size as usize..];
    }

    // Parse central directory
    while let Ok((remaining, header)) = parse_central_directory_header(input) {
        if header.signature != 0x02014b50 {
            break;
        }
        println!("Central Directory Header: {:?}", header);
        input = remaining;
    }

    // Parse end of central directory
    if let Ok((_, eocd)) = parse_end_of_central_directory(input) {
        if eocd.signature == 0x06054b50 {
            println!("End of Central Directory: {:?}", eocd);
        }
    }

    Ok(())
}