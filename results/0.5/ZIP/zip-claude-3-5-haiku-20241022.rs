use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
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
    bit_flag: u16,
    compression_method: u16,
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
struct CentralDirectoryFileHeader {
    version_made_by: u16,
    version_needed: u16,
    bit_flag: u16,
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
    extra_field: Option<Vec<u8>>,
    file_comment: Option<Vec<u8>>,
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    number_of_this_disk: u16,
    disk_with_central_dir: u16,
    total_entries_this_disk: u16,
    total_entries: u16,
    central_directory_size: u32,
    central_directory_offset: u32,
    zip_file_comment_length: u16,
    zip_file_comment: Option<Vec<u8>>,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, _) = tag(&[0x50, 0x4B, 0x03, 0x04])(input)?;
    let (input, (
        version_needed,
        bit_flag,
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        filename_length,
        extra_field_length,
    )) = tuple((
        le_u16, le_u16, le_u16, le_u16, le_u16,
        le_u32, le_u32, le_u32, le_u16, le_u16,
    ))(input)?;

    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = opt(take(extra_field_length))(input)?;

    Ok((input, LocalFileHeader {
        version_needed,
        bit_flag,
        compression_method,
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

fn parse_central_directory_file_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryFileHeader> {
    let (input, _) = tag(&[0x50, 0x4B, 0x01, 0x02])(input)?;
    let (input, (
        version_made_by,
        version_needed,
        bit_flag,
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
    )) = tuple((
        le_u16, le_u16, le_u16, le_u16, le_u16, le_u16,
        le_u32, le_u32, le_u32, le_u16, le_u16, le_u16,
        le_u16, le_u16, le_u32, le_u32,
    ))(input)?;

    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = opt(take(extra_field_length))(input)?;
    let (input, file_comment) = opt(take(file_comment_length))(input)?;

    Ok((input, CentralDirectoryFileHeader {
        version_made_by,
        version_needed,
        bit_flag,
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
        extra_field: extra_field.map(|x| x.to_vec()),
        file_comment: file_comment.map(|x| x.to_vec()),
    }))
}

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (input, _) = tag(&[0x50, 0x4B, 0x05, 0x06])(input)?;
    let (input, (
        number_of_this_disk,
        disk_with_central_dir,
        total_entries_this_disk,
        total_entries,
        central_directory_size,
        central_directory_offset,
        zip_file_comment_length,
    )) = tuple((
        le_u16, le_u16, le_u16, le_u16,
        le_u32, le_u32, le_u16,
    ))(input)?;

    let (input, zip_file_comment) = opt(take(zip_file_comment_length))(input)?;

    Ok((input, EndOfCentralDirectoryRecord {
        number_of_this_disk,
        disk_with_central_dir,
        total_entries_this_disk,
        total_entries,
        central_directory_size,
        central_directory_offset,
        zip_file_comment_length,
        zip_file_comment: zip_file_comment.map(|x| x.to_vec()),
    }))
}

fn parse_zip_file(input: &[u8]) -> IResult<&[u8], (Vec<LocalFileHeader>, Vec<CentralDirectoryFileHeader>, EndOfCentralDirectoryRecord)> {
    let (input, local_headers) = many0(parse_local_file_header)(input)?;
    let (input, central_headers) = many0(parse_central_directory_file_header)(input)?;
    let (input, end_of_central_dir) = parse_end_of_central_directory(input)?;

    Ok((input, (local_headers, central_headers, end_of_central_dir)))
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
        Ok((_, (local_headers, central_headers, end_of_central_dir))) => {
            println!("Local Headers: {}", local_headers.len());
            println!("Central Headers: {}", central_headers.len());
            println!("End of Central Directory: {:?}", end_of_central_dir);
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}