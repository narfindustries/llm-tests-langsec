use nom::{
    bytes::complete::{take, take_till, tag},
    combinator::{map, map_opt, verify},
    error::{context, Error, ErrorKind},
    multi::{length_data, many_till, separated_list1},
    number::complete::{be_u16, be_u32, be_u64},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, seeking::Seek, seeking::SeekFrom};

// ZIP File Signature
fn zip_file_signature(input: &[u8]) -> IResult<&[u8], ()> {
    tag([0x50, 0x4b, 0x03, 0x04])(input).map(|(input, _)| (input, ()))
}

// ZIP End of Central Directory Record
#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    disk_number: u16,
    disk_start: u16,
    num_entries: u16,
    total_entries: u16,
    size: u32,
    offset: u32,
    comment_length: u16,
}

fn end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    context(
        "End Of Central Directory Record",
        tuple((
            tag([0x50, 0x4b, 0x05, 0x06]), // signature
            be_u16, // disk number
            be_u16, // disk start
            be_u16, // num entries
            be_u16, // total entries
            be_u32, // size
            be_u32, // offset
            be_u16, // comment length
        )),
    )(input)
    .map(|(input, (_, disk_number, disk_start, num_entries, total_entries, size, offset, comment_length))| {
        (
            input,
            EndOfCentralDirectoryRecord {
                disk_number,
                disk_start,
                num_entries,
                total_entries,
                size,
                offset,
                comment_length,
            },
        )
    })
}

// ZIP Central Directory Entry
#[derive(Debug)]
struct CentralDirectoryEntry {
    version: u16,
    flags: u16,
    compression_method: u16,
    last_modification_time: u16,
    last_modification_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number: u16,
    internal_file_attributes: u16,
    external_file_attributes: u32,
    local_header_offset: u32,
}

fn central_directory_entry(input: &[u8]) -> IResult<&[u8], CentralDirectoryEntry> {
    context(
        "Central Directory Entry",
        tuple((
            tag([0x50, 0x4b, 0x01, 0x02]), // signature
            be_u16, // version
            be_u16, // flags
            be_u16, // compression method
            be_u16, // last modification time
            be_u16, // last modification date
            be_u32, // crc32
            be_u32, // compressed size
            be_u32, // uncompressed size
            be_u16, // filename length
            be_u16, // extra field length
            be_u16, // file comment length
            be_u16, // disk number
            be_u16, // internal file attributes
            be_u32, // external file attributes
            be_u32, // local header offset
        )),
    )(input)
    .map(|(input, (_, version, flags, compression_method, last_modification_time, last_modification_date, crc32, compressed_size, uncompressed_size, filename_length, extra_field_length, file_comment_length, disk_number, internal_file_attributes, external_file_attributes, local_header_offset))| {
        (
            input,
            CentralDirectoryEntry {
                version,
                flags,
                compression_method,
                last_modification_time,
                last_modification_date,
                crc32,
                compressed_size,
                uncompressed_size,
                filename_length,
                extra_field_length,
                file_comment_length,
                disk_number,
                internal_file_attributes,
                external_file_attributes,
                local_header_offset,
            },
        )
    })
}

// ZIP Local File Header
#[derive(Debug)]
struct LocalFileHeader {
    version: u16,
    flags: u16,
    compression_method: u16,
    last_modification_time: u16,
    last_modification_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
}

fn local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    context(
        "Local File Header",
        tuple((
            tag([0x50, 0x4b, 0x03, 0x04]), // signature
            be_u16, // version
            be_u16, // flags
            be_u16, // compression method
            be_u16, // last modification time
            be_u16, // last modification date
            be_u32, // crc32
            be_u32, // compressed size
            be_u32, // uncompressed size
            be_u16, // filename length
            be_u16, // extra field length
        )),
    )(input)
    .map(|(input, (_, version, flags, compression_method, last_modification_time, last_modification_date, crc32, compressed_size, uncompressed_size, filename_length, extra_field_length))| {
        (
            input,
            LocalFileHeader {
                version,
                flags,
                compression_method,
                last_modification_time,
                last_modification_date,
                crc32,
                compressed_size,
                uncompressed_size,
                filename_length,
                extra_field_length,
            },
        )
    })
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <zipfile>", args[0]);
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let result = zip_file_signature(&buffer);
    match result {
        Ok((remaining, _)) => {
            let result = many_till(
                local_file_header,
                end_of_central_directory_record,
            )(remaining);
            match result {
                Ok((remaining, (headers, eod))) => {
                    for header in headers {
                        println!("{:?}", header);
                    }
                    println!("{:?}", eod);
                }
                Err(err) => {
                    println!("Error parsing ZIP file: {:?}", err);
                }
            }
        }
        Err(err) => {
            println!("Error parsing ZIP file: {:?}", err);
        }
    }
}