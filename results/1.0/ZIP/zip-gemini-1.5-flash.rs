use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, verify},
    error::ErrorKind,
    multi::count,
    number::complete::{le_u16, le_u32, le_u64},
    sequence::{tuple, preceded},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;

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

#[derive(Debug)]
struct ZipEndCentralDirectory {
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
        |(signature, disk_number, start_disk, entries_this_disk, entries_total, size_central_directory, offset_central_directory, comment_length)| ZipEndCentralDirectory {
            signature: signature.into(),
            disk_number,
            start_disk,
            entries_this_disk,
            entries_total,
            size_central_directory,
            offset_central_directory,
            comment_length,
        },
    )(input)
}


fn read_file(path: &Path) -> std::io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}


fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: zip_parser <zip_file>");
        return Ok(());
    }
    let filepath = Path::new(&args[1]);
    let file_bytes = read_file(filepath)?;

    let result = zip_end_central_directory(&file_bytes);
    match result {
        Ok((remaining, ecd)) => {
            println!("End Central Directory: {:?}", ecd);

            let central_directory_offset = ecd.offset_central_directory as usize;
            let central_directory_size = ecd.size_central_directory as usize;
            let central_directory_data = &file_bytes[central_directory_offset..central_directory_offset+central_directory_size];

            for i in 0..ecd.entries_total {
                let res = zip_central_directory_header(central_directory_data);
                match res{
                    Ok((rem, header)) => {
                        println!("Central Directory Entry {}: {:?}", i, header);
                    },
                    Err(e) => {
                        println!("Error parsing central directory entry: {:?}", e);
                        break;
                    }

                }
            }
        }
        Err(e) => {
            println!("Error parsing zip file: {:?}", e);
        }
    }

    Ok(())
}

