use std::env;
use std::fs;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::{tuple, preceded},
    IResult,
};

#[derive(Debug)]
struct ZipCentralDirectoryEntry {
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
    filename: String,
    extra_field: Vec<u8>,
    file_comment: String,
}


fn zip_local_file_header(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag("PK\x03\x04")(input)?;
    let (input, _) = le_u16(input)?;
    let (input, _) = le_u16(input)?;
    let (input, _) = le_u16(input)?;
    let (input, _) = le_u16(input)?;
    let (input, _) = le_u32(input)?;
    let (input, _) = le_u32(input)?;
    let (input, _) = le_u32(input)?;
    let (input, filename_len) = le_u16(input)?;
    let (input, extra_len) = le_u16(input)?;
    let (input, _) = take(filename_len as usize)(input)?;
    let (input, _) = take(extra_len as usize)(input)?;
    Ok((input, ()))

}

fn zip_central_directory_entry(input: &[u8]) -> IResult<&[u8], ZipCentralDirectoryEntry> {
    let (input, _) = tag("PK\x01\x02")(input)?;
    let (input, version_made_by) = le_u16(input)?;
    let (input, version_needed) = le_u16(input)?;
    let (input, bit_flag) = le_u16(input)?;
    let (input, compression_method) = le_u16(input)?;
    let (input, last_mod_time) = le_u16(input)?;
    let (input, last_mod_date) = le_u16(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, compressed_size) = le_u32(input)?;
    let (input, uncompressed_size) = le_u32(input)?;
    let (input, filename_length) = le_u16(input)?;
    let (input, extra_field_length) = le_u16(input)?;
    let (input, file_comment_length) = le_u16(input)?;
    let (input, disk_number_start) = le_u16(input)?;
    let (input, internal_file_attributes) = le_u16(input)?;
    let (input, external_file_attributes) = le_u32(input)?;
    let (input, local_header_offset) = le_u32(input)?;
    let (input, filename) = map_res(take(filename_length as usize), |bytes| std::str::from_utf8(bytes).map(|s| s.to_string()))(input)?;
    let (input, extra_field) = take(extra_field_length as usize)(input)?;
    let (input, file_comment) = map_res(take(file_comment_length as usize), |bytes| std::str::from_utf8(bytes).map(|s| s.to_string()))(input)?;

    Ok((
        input,
        ZipCentralDirectoryEntry {
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
            filename,
            extra_field: extra_field.to_vec(),
            file_comment,
        },
    ))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = fs::File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let result = zip_central_directory_entry(&buffer);

    match result {
        Ok((_, entry)) => println!("{:#?}", entry),
        Err(e) => eprintln!("Error parsing ZIP file: {:?}", e),
    }
}
