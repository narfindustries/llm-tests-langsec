use std::env;
use std::fs;
use std::path::Path;
use nom::{
    IResult,
    number::complete::{be_u16, be_u32, be_u64},
    bytes::complete::{take, tag},
    combinator::{map, map_res, all_consuming},
    sequence::tuple,
    multi::count,
};

#[derive(Debug)]
struct ZipLocalFileHeader {
    signature: u32,
    version: u16,
    flags: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
}


#[derive(Debug)]
struct ZipCentralDirectoryEntry {
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
    internal_file_attributes: u16,
    external_file_attributes: u32,
    local_header_offset: u32,
}

#[derive(Debug)]
struct ZipEndOfCentralDirectory {
    signature: u32,
    disk_number: u16,
    start_disk: u16,
    entries_this_disk: u16,
    entries_total: u16,
    size_central_directory: u32,
    offset_central_directory: u32,
    comment_length: u16,
}

fn zip_local_file_header(input: &[u8]) -> IResult<&[u8], ZipLocalFileHeader> {
    map(
        tuple((
            be_u32,
            be_u16,
            be_u16,
            be_u16,
            be_u16,
            be_u16,
            be_u32,
            be_u32,
            be_u32,
            be_u16,
            be_u16,
        )),
        |(signature, version, flags, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, filename_length, extra_field_length)|
            ZipLocalFileHeader {
                signature,
                version,
                flags,
                compression_method,
                last_mod_time,
                last_mod_date,
                crc32,
                compressed_size,
                uncompressed_size,
                filename_length,
                extra_field_length,
            },
    )(input)
}


fn zip_central_directory_entry(input: &[u8]) -> IResult<&[u8], ZipCentralDirectoryEntry> {
    map(
        tuple((
            be_u32, be_u16, be_u16, be_u16, be_u16, be_u16, be_u16, be_u32, be_u32, be_u32, be_u16, be_u16, be_u16, be_u16, be_u16, be_u32, be_u32
        )),
        |(signature, version_made_by, version_needed, flags, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, filename_length, extra_field_length, file_comment_length, disk_number_start, internal_file_attributes, external_file_attributes, local_header_offset)| ZipCentralDirectoryEntry {
            signature, version_made_by, version_needed, flags, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, filename_length, extra_field_length, file_comment_length, disk_number_start, internal_file_attributes, external_file_attributes, local_header_offset
        }
    )(input)
}

fn zip_end_of_central_directory(input: &[u8]) -> IResult<&[u8], ZipEndOfCentralDirectory> {
    map(
        tuple((
            be_u32, be_u16, be_u16, be_u16, be_u16, be_u32, be_u32, be_u16,
        )),
        |(signature, disk_number, start_disk, entries_this_disk, entries_total, size_central_directory, offset_central_directory, comment_length)| ZipEndOfCentralDirectory {
            signature, disk_number, start_disk, entries_this_disk, entries_total, size_central_directory, offset_central_directory, comment_length
        }
    )(input)
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: zip_parser <zip_file>");
        return;
    }

    let filename = &args[1];
    let path = Path::new(filename);

    match fs::read(path) {
        Ok(bytes) => {
            match all_consuming(zip_end_of_central_directory)(&bytes) {
                Ok((_, eocd)) => println!("End of Central Directory: {:?}", eocd),
                Err(e) => println!("Error parsing EOCD: {:?}", e),
            }
        },
        Err(e) => println!("Error reading file: {}", e),
    }
}
