use nom::{
    IResult, bytes::complete::{take, tag}, number::complete::{le_u16, le_u32}, sequence::tuple, multi::count
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug)]
struct LocalFileHeader {
    version_needed: u16,
    gp_bit_flag: u16,
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
struct CentralDirectoryFileHeader {
    version_made_by: u16,
    version_needed: u16,
    gp_bit_flag: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    comment_length: u16,
    disk_number_start: u16,
    internal_file_attributes: u16,
    external_file_attributes: u32,
    relative_offset: u32,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    number_disk: u16,
    disk_cd_starts: u16,
    num_cd_records_on_disk: u16,
    total_cd_records: u16,
    size_of_cd: u32,
    offset_of_cd: u32,
    zip_file_comment_length: u16,
    zip_file_comment: Vec<u8>,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, (_signature, version_needed, gp_bit_flag, compression_method,
                 last_mod_time, last_mod_date, crc32, compressed_size,
                 uncompressed_size, file_name_length, extra_field_length)) =
        tuple((tag(b"PK\x03\x04"), le_u16, le_u16, le_u16, le_u16, le_u16, le_u32,
               le_u32, le_u32, le_u16, le_u16))(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;

    Ok((input, LocalFileHeader {
        version_needed, gp_bit_flag, compression_method, last_mod_time, last_mod_date,
        crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length,
        file_name: file_name.to_vec(), extra_field: extra_field.to_vec(),
    }))
}

fn parse_central_directory_file_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryFileHeader> {
    let (input, (_signature, version_made_by, version_needed, gp_bit_flag, compression_method,
                 last_mod_time, last_mod_date, crc32, compressed_size,
                 uncompressed_size, file_name_length, extra_field_length,
                 comment_length, disk_number_start, internal_file_attributes,
                 external_file_attributes, relative_offset)) =
        tuple((tag(b"PK\x01\x02"), le_u16, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32,
               le_u32, le_u32, le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32))(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(comment_length)(input)?;

    Ok((input, CentralDirectoryFileHeader {
        version_made_by, version_needed, gp_bit_flag, compression_method, last_mod_time, last_mod_date,
        crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length, comment_length,
        disk_number_start, internal_file_attributes, external_file_attributes, relative_offset,
        file_name: file_name.to_vec(), extra_field: extra_field.to_vec(), file_comment: file_comment.to_vec(),
    }))
}

fn parse_end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (input, (_signature, number_disk, disk_cd_starts, num_cd_records_on_disk, total_cd_records,
                 size_of_cd, offset_of_cd, zip_file_comment_length)) =
        tuple((tag(b"PK\x05\x06"), le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u16))(input)?;
    let (input, zip_file_comment) = take(zip_file_comment_length)(input)?;

    Ok((input, EndOfCentralDirectoryRecord {
        number_disk, disk_cd_starts, num_cd_records_on_disk, total_cd_records,
        size_of_cd, offset_of_cd, zip_file_comment_length, zip_file_comment: zip_file_comment.to_vec(),
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} <zip_file>", args[0]);
        return;
    }
    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(f) => f,
        Err(e) => {
            println!("Failed to open file: {}", e);
            return;
        }
    };

    let mut data = Vec::new();
    if let Err(e) = file.read_to_end(&mut data) {
        println!("Failed to read file: {}", e);
        return;
    }

    match parse_end_of_central_directory_record(&data) {
        Ok((_remaining, eocdr)) => {
            println!("Parsed End of Central Directory Record: {:?}", eocdr);
        },
        Err(e) => println!("Failed to parse: {:?}", e),
    }
}