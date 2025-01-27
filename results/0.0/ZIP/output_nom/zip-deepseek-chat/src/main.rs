use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::{
    fs::File,
    io::{self, Read},
    path::Path,
};

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
    file_name_length: u16,
    extra_field_length: u16,
    file_name: String,
    extra_field: Vec<u8>,
}

#[derive(Debug)]
struct CentralDirectoryHeader {
    version_made_by: u16,
    version_needed: u16,
    bit_flag: u16,
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
    internal_attributes: u16,
    external_attributes: u32,
    relative_offset: u32,
    file_name: String,
    extra_field: Vec<u8>,
    file_comment: String,
}

#[derive(Debug)]
struct EndOfCentralDirectory {
    disk_number: u16,
    central_directory_disk: u16,
    num_central_directory_records_on_disk: u16,
    total_central_directory_records: u16,
    central_directory_size: u32,
    central_directory_offset: u32,
    zip_file_comment_length: u16,
    zip_file_comment: String,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, (version_needed, bit_flag, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length)) = tuple((
        le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16,
    ))(input)?;

    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;

    Ok((
        input,
        LocalFileHeader {
            version_needed,
            bit_flag,
            compression_method,
            last_mod_time,
            last_mod_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name_length,
            extra_field_length,
            file_name: String::from_utf8_lossy(file_name).to_string(),
            extra_field: extra_field.to_vec(),
        },
    ))
}

fn parse_central_directory_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryHeader> {
    let (input, (version_made_by, version_needed, bit_flag, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length, file_comment_length, disk_number_start, internal_attributes, external_attributes, relative_offset)) = tuple((
        le_u16, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32,
    ))(input)?;

    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    Ok((
        input,
        CentralDirectoryHeader {
            version_made_by,
            version_needed,
            bit_flag,
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
            internal_attributes,
            external_attributes,
            relative_offset,
            file_name: String::from_utf8_lossy(file_name).to_string(),
            extra_field: extra_field.to_vec(),
            file_comment: String::from_utf8_lossy(file_comment).to_string(),
        },
    ))
}

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectory> {
    let (input, (disk_number, central_directory_disk, num_central_directory_records_on_disk, total_central_directory_records, central_directory_size, central_directory_offset, zip_file_comment_length)) = tuple((
        le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u16,
    ))(input)?;

    let (input, zip_file_comment) = take(zip_file_comment_length)(input)?;

    Ok((
        input,
        EndOfCentralDirectory {
            disk_number,
            central_directory_disk,
            num_central_directory_records_on_disk,
            total_central_directory_records,
            central_directory_size,
            central_directory_offset,
            zip_file_comment_length,
            zip_file_comment: String::from_utf8_lossy(zip_file_comment).to_string(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <zip file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    if let Ok((_, local_file_header)) = parse_local_file_header(&buffer) {
        println!("{:?}", local_file_header);
    }

    if let Ok((_, central_directory_header)) = parse_central_directory_header(&buffer) {
        println!("{:?}", central_directory_header);
    }

    if let Ok((_, end_of_central_directory)) = parse_end_of_central_directory(&buffer) {
        println!("{:?}", end_of_central_directory);
    }

    Ok(())
}