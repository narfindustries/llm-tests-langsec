use nom::{
    bytes::complete::{take, tag},
    combinator::{map, map_res, opt, verify},
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::str;

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
    filename: String,
    extra_field: Vec<u8>,
}

#[derive(Debug)]
struct CentralDirectoryEntry {
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
    file_comment_length: u16,
    disk_number_start: u16,
    internal_attributes: u16,
    external_attributes: u32,
    local_header_offset: u32,
    filename: String,
    extra_field: Vec<u8>,
    file_comment: String,
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    signature: u32,
    disk_number: u16,
    disk_with_cd: u16,
    num_entries_on_disk: u16,
    num_entries_total: u16,
    cd_size: u32,
    cd_offset: u32,
    comment_length: u16,
    comment: String,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, (signature, version_needed, flags, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, filename_length, extra_field_length)) = tuple((
        le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16,
    ))(input)?;

    let (input, filename) = take(filename_length)(input)?;
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
            filename_length,
            extra_field_length,
            filename: String::from_utf8_lossy(filename).to_string(),
            extra_field: extra_field.to_vec(),
        },
    ))
}

fn parse_central_directory_entry(input: &[u8]) -> IResult<&[u8], CentralDirectoryEntry> {
    let (input, (signature, version_made_by, version_needed, flags, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, filename_length, extra_field_length, file_comment_length, disk_number_start, internal_attributes, external_attributes, local_header_offset)) = tuple((
        le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32,
    ))(input)?;

    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    Ok((
        input,
        CentralDirectoryEntry {
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
            file_comment_length,
            disk_number_start,
            internal_attributes,
            external_attributes,
            local_header_offset,
            filename: String::from_utf8_lossy(filename).to_string(),
            extra_field: extra_field.to_vec(),
            file_comment: String::from_utf8_lossy(file_comment).to_string(),
        },
    ))
}

fn parse_end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (input, (signature, disk_number, disk_with_cd, num_entries_on_disk, num_entries_total, cd_size, cd_offset, comment_length)) = tuple((
        le_u32, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u16,
    ))(input)?;

    let (input, comment) = take(comment_length)(input)?;

    Ok((
        input,
        EndOfCentralDirectoryRecord {
            signature,
            disk_number,
            disk_with_cd,
            num_entries_on_disk,
            num_entries_total,
            cd_size,
            cd_offset,
            comment_length,
            comment: String::from_utf8_lossy(comment).to_string(),
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let result = parse_end_of_central_directory_record(&buffer);

    match result {
        Ok((_remaining, ecdr)) => {
            println!("End of Central Directory Record: {:?}", ecdr);
            //Further processing of central directory entries based on ecdr.cd_offset and ecdr.cd_size
        }
        Err(e) => eprintln!("Error parsing ZIP file: {:?}", e),
    }
}

