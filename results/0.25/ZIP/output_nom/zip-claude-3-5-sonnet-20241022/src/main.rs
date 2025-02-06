use nom::{
    bytes::complete::{take},
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
    let (input, central_dir_disk) = le_u16(input)?;
    let (input, disk_entries) = le_u16(input)?;
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
            central_dir_disk,
            disk_entries,
            total_entries,
            central_dir_size,
            central_dir_offset,
            comment_length,
            comment: comment.to_vec(),
        },
    ))
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

    let mut current_pos = 0;
    let mut local_headers = Vec::new();
    let mut central_headers = Vec::new();

    // Parse local file headers
    while current_pos < buffer.len() {
        if let Ok((remaining, header)) = parse_local_file_header(&buffer[current_pos..]) {
            if header.signature == 0x04034b50 {
                current_pos = buffer.len() - remaining.len();
                current_pos += header.compressed_size as usize;
                local_headers.push(header);
            } else {
                break;
            }
        } else {
            break;
        }
    }

    // Parse central directory headers
    while current_pos < buffer.len() {
        if let Ok((remaining, header)) = parse_central_directory_header(&buffer[current_pos..]) {
            if header.signature == 0x02014b50 {
                current_pos = buffer.len() - remaining.len();
                central_headers.push(header);
            } else {
                break;
            }
        } else {
            break;
        }
    }

    // Parse end of central directory
    if let Ok((_, end_of_central_dir)) = parse_end_of_central_directory(&buffer[current_pos..]) {
        println!("Local File Headers: {:#?}", local_headers);
        println!("Central Directory Headers: {:#?}", central_headers);
        println!("End of Central Directory: {:#?}", end_of_central_dir);
    }

    Ok(())
}