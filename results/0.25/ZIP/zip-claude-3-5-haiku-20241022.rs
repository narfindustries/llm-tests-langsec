use nom::{
    bytes::complete::{tag, take, take_until},
    combinator::{map, opt},
    multi::many0,
    number::complete::{le_u16, le_u32},
    sequence::{tuple, preceded},
    IResult,
};
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
    filename_length: u16,
    extra_field_length: u16,
    filename: String,
    extra_field: Option<Vec<u8>>,
    file_data: Vec<u8>,
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
    filename: String,
    extra_field: Option<Vec<u8>>,
    file_comment: Option<String>,
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
    comment: Option<String>,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], ZipLocalFileHeader> {
    let (input, _) = tag(b"PK\x03\x04")(input)?;
    let (input, (version_needed, flags, compression_method, last_mod_time, last_mod_date, 
                 crc32, compressed_size, uncompressed_size, filename_length, extra_field_length)) = 
        tuple((
            le_u16, le_u16, le_u16, le_u16, le_u16, 
            le_u32, le_u32, le_u32, le_u16, le_u16
        ))(input)?;

    let (input, filename) = map(take(filename_length as usize), |bytes: &[u8]| 
        String::from_utf8_lossy(bytes).into_owned()
    )(input)?;

    let (input, extra_field) = opt(map(take(extra_field_length as usize), |bytes: &[u8]| bytes.to_vec()))(input)?;

    let (input, file_data) = take(compressed_size as usize)(input)?;

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
        filename,
        extra_field,
        file_data: file_data.to_vec(),
    }))
}

fn parse_central_directory_file_header(input: &[u8]) -> IResult<&[u8], ZipCentralDirectoryFileHeader> {
    let (input, _) = tag(b"PK\x01\x02")(input)?;
    let (input, (version_made_by, version_needed, flags, compression_method, 
                 last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, 
                 filename_length, extra_field_length, file_comment_length, 
                 disk_number_start, internal_file_attributes, external_file_attributes, 
                 local_header_offset)) = 
        tuple((
            le_u16, le_u16, le_u16, le_u16, le_u16, le_u16, 
            le_u32, le_u32, le_u32, le_u16, le_u16, le_u16, 
            le_u16, le_u16, le_u32, le_u32
        ))(input)?;

    let (input, filename) = map(take(filename_length as usize), |bytes: &[u8]| 
        String::from_utf8_lossy(bytes).into_owned()
    )(input)?;

    let (input, extra_field) = opt(map(take(extra_field_length as usize), |bytes: &[u8]| bytes.to_vec()))(input)?;

    let (input, file_comment) = opt(map(take(file_comment_length as usize), |bytes: &[u8]| 
        String::from_utf8_lossy(bytes).into_owned()
    ))(input)?;

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
        filename,
        extra_field,
        file_comment,
    }))
}

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], ZipEndOfCentralDirectory> {
    let (input, _) = tag(b"PK\x05\x06")(input)?;
    let (input, (number_of_this_disk, disk_with_central_directory, 
                 total_entries_this_disk, total_entries, 
                 central_directory_size, central_directory_offset, 
                 comment_length)) = 
        tuple((
            le_u16, le_u16, le_u16, le_u16, 
            le_u32, le_u32, le_u16
        ))(input)?;

    let (input, comment) = opt(map(take(comment_length as usize), |bytes: &[u8]| 
        String::from_utf8_lossy(bytes).into_owned()
    ))(input)?;

    Ok((input, ZipEndOfCentralDirectory {
        number_of_this_disk,
        disk_with_central_directory,
        total_entries_this_disk,
        total_entries,
        central_directory_size,
        central_directory_offset,
        comment_length,
        comment,
    }))
}

fn parse_zip_file(input: &[u8]) -> IResult<&[u8], (Vec<ZipLocalFileHeader>, Vec<ZipCentralDirectoryFileHeader>, ZipEndOfCentralDirectory)> {
    let (input, local_headers) = many0(parse_local_file_header)(input)?;
    let (input, central_headers) = many0(parse_central_directory_file_header)(input)?;
    let (input, end_of_central_directory) = parse_end_of_central_directory(input)?;

    Ok((input, (local_headers, central_headers, end_of_central_directory)))
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

    match parse_zip_file(&buffer) {
        Ok((_, (local_headers, central_headers, end_of_central_directory))) => {
            println!("Local File Headers: {:?}", local_headers);
            println!("Central Directory File Headers: {:?}", central_headers);
            println!("End of Central Directory: {:?}", end_of_central_directory);
        }
        Err(e) => {
            eprintln!("Error parsing ZIP file: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}