use std::env;
use std::fs;
use std::path::Path;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    number::complete::{le_u16, le_u32, le_u64},
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
struct ZipCentralDirectoryHeader {
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
            signature: signature.into(),
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

fn zip_central_directory_header(input: &[u8]) -> IResult<&[u8], ZipCentralDirectoryHeader> {
    map(
        tuple((
            tag(b"\x50\x4b\x01\x02"),
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
            le_u32,
        )),
        |(signature, version_made_by, version_needed, flags, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, filename_length, extra_field_length, file_comment_length, disk_number_start, internal_file_attributes, external_file_attributes, local_header_offset)| ZipCentralDirectoryHeader {
            signature: signature.into(),
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
        },
    )(input)
}


fn read_zip_file(path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let data = fs::read(path)?;
    let (_, header) = zip_central_directory_header(&data)?;
    println!("{:?}", header);
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: zip_parser <zip_file>");
        std::process::exit(1);
    }
    let path = Path::new(&args[1]);
    read_zip_file(path)?;
    Ok(())
}
