use nom::{
    bytes::complete::{tag, take},
    combinator::{map, rest},
    multi::{count, many_till},
    number::complete::{le_u16, le_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct LocalFileHeader {
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
    extra_field: Vec<u8>,
    data: Vec<u8>,
}

#[derive(Debug)]
struct CentralDirectoryFileHeader {
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
    comment_length: u16,
    disk_number_start: u16,
    internal_file_attributes: u16,
    external_file_attributes: u32,
    local_header_offset: u32,
    filename: Vec<u8>,
    extra_field: Vec<u8>,
    comment: Vec<u8>,
}

#[derive(Debug)]
struct EndOfCentralDirectory {
    disk_number: u16,
    central_directory_disk: u16,
    total_entries_this_disk: u16,
    total_entries: u16,
    central_directory_size: u32,
    central_directory_offset: u32,
    comment_length: u16,
    comment: Vec<u8>,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, _) = tag(b"PK\x03\x04")(input)?;
    let (input, (version_needed, flags, compression, last_mod_time, last_mod_date, 
                 crc32, compressed_size, uncompressed_size, filename_length, extra_field_length)) = 
        tuple((
            le_u16, le_u16, le_u16, le_u16, le_u16, 
            le_u32, le_u32, le_u32, le_u16, le_u16
        ))(input)?;
    
    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, data) = take(compressed_size)(input)?;
    
    Ok((input, LocalFileHeader {
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
        extra_field: extra_field.to_vec(),
        data: data.to_vec(),
    }))
}

fn parse_central_directory_file_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryFileHeader> {
    let (input, _) = tag(b"PK\x01\x02")(input)?;
    let (input, (version_made_by, version_needed, flags, compression, last_mod_time, last_mod_date, 
                 crc32, compressed_size, uncompressed_size, filename_length, extra_field_length,
                 comment_length, disk_number_start, internal_file_attributes, external_file_attributes,
                 local_header_offset)) = 
        tuple((
            le_u16, le_u16, le_u16, le_u16, le_u16, le_u16, 
            le_u32, le_u32, le_u32, le_u16, le_u16, le_u16, 
            le_u16, le_u16, le_u32, le_u32
        ))(input)?;
    
    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, comment) = take(comment_length)(input)?;
    
    Ok((input, CentralDirectoryFileHeader {
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
        comment_length,
        disk_number_start,
        internal_file_attributes,
        external_file_attributes,
        local_header_offset,
        filename: filename.to_vec(),
        extra_field: extra_field.to_vec(),
        comment: comment.to_vec(),
    }))
}

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectory> {
    let (input, _) = tag(b"PK\x05\x06")(input)?;
    let (input, (disk_number, central_directory_disk, total_entries_this_disk, total_entries,
                 central_directory_size, central_directory_offset, comment_length)) = 
        tuple((
            le_u16, le_u16, le_u16, le_u16, 
            le_u32, le_u32, le_u16
        ))(input)?;
    
    let (input, comment) = take(comment_length)(input)?;
    
    Ok((input, EndOfCentralDirectory {
        disk_number,
        central_directory_disk,
        total_entries_this_disk,
        total_entries,
        central_directory_size,
        central_directory_offset,
        comment_length,
        comment: comment.to_vec(),
    }))
}

fn parse_zip_file(input: &[u8]) -> IResult<&[u8], (Vec<LocalFileHeader>, Vec<CentralDirectoryFileHeader>, EndOfCentralDirectory)> {
    let (input, local_headers) = many_till(parse_local_file_header, tag(b"PK\x01\x02"))(input)?;
    let (input, (central_headers, _)) = many_till(parse_central_directory_file_header, tag(b"PK\x05\x06"))(input)?;
    let (input, end_of_central_directory) = parse_end_of_central_directory(input)?;
    
    Ok((input, (local_headers.0, central_headers.0, end_of_central_directory)))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_zip_file(&buffer) {
        Ok((_, (local_headers, central_headers, end_of_central_directory))) => {
            println!("Parsed ZIP file successfully!");
            println!("Local File Headers: {}", local_headers.len());
            println!("Central Directory Headers: {}", central_headers.len());
            println!("End of Central Directory: {:?}", end_of_central_directory);
        },
        Err(e) => {
            eprintln!("Failed to parse ZIP file: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}