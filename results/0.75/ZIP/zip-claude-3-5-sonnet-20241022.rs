use std::fs;
use std::env;
use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};

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
    file_name_length: u16,
    extra_field_length: u16,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
}

#[derive(Debug)]
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

#[derive(Debug)]
struct EndOfCentralDirectory {
    signature: u32,
    disk_number: u16,
    start_disk_number: u16,
    entries_on_disk: u16,
    total_entries: u16,
    central_dir_size: u32,
    central_dir_offset: u32,
    comment_length: u16,
    comment: Vec<u8>,
}

#[derive(Debug)]
struct DataDescriptor {
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, (
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
        extra_field_length
    )) = tuple((
        le_u32,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u32,
        le_u32,
        le_u32,
        le_u16,
        le_u16
    ))(input)?;

    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;

    Ok((input, LocalFileHeader {
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
    }))
}

fn parse_central_directory_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryHeader> {
    let (input, (
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
        local_header_offset
    )) = tuple((
        le_u32,
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
        le_u32
    ))(input)?;

    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;

    Ok((input, CentralDirectoryHeader {
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
    }))
}

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectory> {
    let (input, (
        signature,
        disk_number,
        start_disk_number,
        entries_on_disk,
        total_entries,
        central_dir_size,
        central_dir_offset,
        comment_length
    )) = tuple((
        le_u32,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u32,
        le_u32,
        le_u16
    ))(input)?;

    let (input, comment) = take(comment_length)(input)?;

    Ok((input, EndOfCentralDirectory {
        signature,
        disk_number,
        start_disk_number,
        entries_on_disk,
        total_entries,
        central_dir_size,
        central_dir_offset,
        comment_length,
        comment: comment.to_vec(),
    }))
}

fn parse_data_descriptor(input: &[u8]) -> IResult<&[u8], DataDescriptor> {
    let (input, (crc32, compressed_size, uncompressed_size)) = tuple((
        le_u32,
        le_u32,
        le_u32
    ))(input)?;

    Ok((input, DataDescriptor {
        crc32,
        compressed_size,
        uncompressed_size,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        return;
    }

    let data = match fs::read(&args[1]) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };

    // Find end of central directory
    let eocd_signature = 0x06054b50u32.to_le_bytes();
    if let Some(pos) = data.windows(4).rposition(|window| window == &eocd_signature) {
        let eocd_data = &data[pos..];
        match parse_end_of_central_directory(eocd_data) {
            Ok((_, eocd)) => {
                println!("End of Central Directory found:");
                println!("{:#?}", eocd);

                // Parse central directory entries
                let cd_start = eocd.central_dir_offset as usize;
                let mut cd_data = &data[cd_start..pos];
                
                for _ in 0..eocd.total_entries {
                    if let Ok((remaining, cd_header)) = parse_central_directory_header(cd_data) {
                        println!("\nCentral Directory Header:");
                        println!("{:#?}", cd_header);
                        cd_data = remaining;

                        // Parse corresponding local file header
                        let local_start = cd_header.local_header_offset as usize;
                        if let Ok((_, local_header)) = parse_local_file_header(&data[local_start..]) {
                            println!("\nLocal File Header:");
                            println!("{:#?}", local_header);

                            // Parse data descriptor if present
                            if cd_header.flags & 0x0008 != 0 {
                                let desc_start = local_start + 30 + 
                                    local_header.file_name_length as usize +
                                    local_header.extra_field_length as usize +
                                    local_header.compressed_size as usize;
                                if let Ok((_, data_desc)) = parse_data_descriptor(&data[desc_start..]) {
                                    println!("\nData Descriptor:");
                                    println!("{:#?}", data_desc);
                                }
                            }
                        }
                    }
                }
            }
            Err(e) => eprintln!("Error parsing EOCD: {:?}", e),
        }
    } else {
        eprintln!("End of Central Directory signature not found");
    }
}