use std::env;
use std::fs;
use std::path::Path;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    number::complete::le_u16,
    sequence::{tuple, preceded},
    IResult,
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
struct ZipEndCentralDirectory {
    signature: u32,
    disk_number: u16,
    start_disk_number: u16,
    entries_this_disk: u16,
    entries_total: u16,
    size_central_directory: u32,
    offset_central_directory: u32,
    comment_length: u16,
}

fn zip_local_file_header(input: &[u8]) -> IResult<&[u8], ZipLocalFileHeader> {
    map(
        tuple((
            tag(b"\x50\x4b\x03\x04"),
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
        )),
        |(signature, version, flags, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, filename_length, extra_field_length)| ZipLocalFileHeader {
            signature: u32::from_be_bytes(signature.try_into().unwrap()),
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
        }
    )(input)
}

fn zip_end_central_directory(input: &[u8]) -> IResult<&[u8], ZipEndCentralDirectory> {
    map(
        tuple((
            tag(b"\x50\x4b\x05\x06"),
            le_u16,
            le_u16,
            le_u16,
            le_u16,
            le_u32,
            le_u32,
            le_u16,
        )),
        |(signature, disk_number, start_disk_number, entries_this_disk, entries_total, size_central_directory, offset_central_directory, comment_length)| ZipEndCentralDirectory {
            signature: u32::from_be_bytes(signature.try_into().unwrap()),
            disk_number,
            start_disk_number,
            entries_this_disk,
            entries_total,
            size_central_directory,
            offset_central_directory,
            comment_length,
        }
    )(input)
}


fn parse_zip(input: &[u8]) -> IResult<&[u8], (ZipLocalFileHeader, ZipEndCentralDirectory)> {
    let (input, header) = zip_local_file_header(input)?;
    let (input, _) = take(header.filename_length as usize)(input)?;
    let (input, _) = take(header.extra_field_length as usize)(input)?;
    let (input, _) = take(header.compressed_size as usize)(input)?;
    let (input, footer) = preceded(opt(take(1024)), zip_end_central_directory)(input)?;
    Ok((input, (header, footer)))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let path = Path::new(filename);

    match fs::read(path) {
        Ok(bytes) => {
            match parse_zip(&bytes) {
                Ok((_, (header, footer))) => {
                    println!("Local File Header: {:?}", header);
                    println!("End Central Directory: {:?}", footer);
                }
                Err(e) => {
                    eprintln!("Error parsing ZIP file: {:?}", e);
                }
            }
        }
        Err(e) => {
            eprintln!("Error reading file: {}", e);
        }
    }
}
