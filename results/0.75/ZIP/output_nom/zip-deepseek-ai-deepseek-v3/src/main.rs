use nom::{
    bytes::complete::take,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct LocalFileHeader {
    signature: u32,
    version_needed: u16,
    bit_flag: u16,
    compression_method: u16,
    mod_time: u16,
    mod_date: u16,
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
    bit_flag: u16,
    compression_method: u16,
    mod_time: u16,
    mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    internal_attrs: u16,
    external_attrs: u32,
    relative_offset: u32,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

#[derive(Debug)]
struct EndOfCentralDirectory {
    signature: u32,
    disk_number: u16,
    start_disk: u16,
    num_records_on_disk: u16,
    total_records: u16,
    central_directory_size: u32,
    central_directory_offset: u32,
    zip_comment_length: u16,
    zip_comment: Vec<u8>,
}

#[derive(Debug)]
struct DataDescriptor {
    signature: u32,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, (signature, version_needed, bit_flag, compression_method, mod_time, mod_date, crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length)) = tuple((
        le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16,
    ))(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    Ok((
        input,
        LocalFileHeader {
            signature,
            version_needed,
            bit_flag,
            compression_method,
            mod_time,
            mod_date,
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
    let (input, (signature, version_made_by, version_needed, bit_flag, compression_method, mod_time, mod_date, crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length, file_comment_length, disk_number_start, internal_attrs, external_attrs, relative_offset)) = tuple((
        le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32,
    ))(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;
    Ok((
        input,
        CentralDirectoryHeader {
            signature,
            version_made_by,
            version_needed,
            bit_flag,
            compression_method,
            mod_time,
            mod_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name_length,
            extra_field_length,
            file_comment_length,
            disk_number_start,
            internal_attrs,
            external_attrs,
            relative_offset,
            file_name: file_name.to_vec(),
            extra_field: extra_field.to_vec(),
            file_comment: file_comment.to_vec(),
        },
    ))
}

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectory> {
    let (input, (signature, disk_number, start_disk, num_records_on_disk, total_records, central_directory_size, central_directory_offset, zip_comment_length)) = tuple((
        le_u32, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u16,
    ))(input)?;
    let (input, zip_comment) = take(zip_comment_length)(input)?;
    Ok((
        input,
        EndOfCentralDirectory {
            signature,
            disk_number,
            start_disk,
            num_records_on_disk,
            total_records,
            central_directory_size,
            central_directory_offset,
            zip_comment_length,
            zip_comment: zip_comment.to_vec(),
        },
    ))
}

fn parse_data_descriptor(input: &[u8]) -> IResult<&[u8], DataDescriptor> {
    let (input, (signature, crc32, compressed_size, uncompressed_size)) = tuple((
        le_u32, le_u32, le_u32, le_u32,
    ))(input)?;
    Ok((
        input,
        DataDescriptor {
            signature,
            crc32,
            compressed_size,
            uncompressed_size,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    let mut remaining = &data[..];

    while !remaining.is_empty() {
        if remaining.len() >= 4 {
            let signature = u32::from_le_bytes(remaining[..4].try_into().unwrap());
            match signature {
                0x04034b50 => {
                    let (new_remaining, local_file_header) = parse_local_file_header(remaining).unwrap();
                    println!("Local File Header: {:?}", local_file_header);
                    remaining = new_remaining;
                }
                0x02014b50 => {
                    let (new_remaining, central_dir_header) = parse_central_directory_header(remaining).unwrap();
                    println!("Central Directory Header: {:?}", central_dir_header);
                    remaining = new_remaining;
                }
                0x06054b50 => {
                    let (new_remaining, end_of_central_dir) = parse_end_of_central_directory(remaining).unwrap();
                    println!("End of Central Directory: {:?}", end_of_central_dir);
                    remaining = new_remaining;
                }
                0x08074b50 => {
                    let (new_remaining, data_descriptor) = parse_data_descriptor(remaining).unwrap();
                    println!("Data Descriptor: {:?}", data_descriptor);
                    remaining = new_remaining;
                }
                _ => {
                    remaining = &remaining[1..];
                }
            }
        } else {
            break;
        }
    }
}