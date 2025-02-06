use nom::{
    bytes::complete::take,
    combinator::map,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::process;
use std::str;

#[derive(Debug)]
struct LocalFileHeader {
    signature: u32,
    version_needed: u16,
    general_purpose_bit_flag: u16,
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
struct CentralDirectoryHeader {
    signature: u32,
    version_made_by: u16,
    version_needed: u16,
    general_purpose_bit_flag: u16,
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
    relative_offset_local_header: u32,
    filename: String,
    extra_field: Vec<u8>,
    file_comment: String,
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    signature: u32,
    disk_number: u16,
    disk_number_with_cd: u16,
    num_entries_on_disk: u16,
    num_entries_total: u16,
    size_of_cd: u32,
    offset_of_cd: u32,
    comment_length: u16,
    comment: String,
}

fn read_zip_file(filename: &str) -> Result<Vec<u8>, std::io::Error> {
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn parse_local_file_header(i: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (i, (signature, version_needed, general_purpose_bit_flag, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, filename_length, extra_field_length, filename, extra_field)) = tuple((
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
        le_u16,
        map(take, |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned()),
        take,
    ))(i)?;
    Ok((
        i,
        LocalFileHeader {
            signature,
            version_needed,
            general_purpose_bit_flag,
            compression_method,
            last_mod_time,
            last_mod_date,
            crc32,
            compressed_size,
            uncompressed_size,
            filename_length,
            extra_field_length,
            filename,
            extra_field: extra_field.to_vec(),
        },
    ))
}

fn parse_central_directory_header(i: &[u8]) -> IResult<&[u8], CentralDirectoryHeader> {
    let (i, (signature, version_made_by, version_needed, general_purpose_bit_flag, compression_method, last_mod_time, last_mod_date, crc32, compressed_size, uncompressed_size, filename_length, extra_field_length, file_comment_length, disk_number_start, internal_file_attributes, external_file_attributes, relative_offset_local_header, filename, extra_field, file_comment)) = tuple((
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
        le_u32,
        map(take, |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned()),
        take,
        map(take, |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned()),
    ))(i)?;
    Ok((
        i,
        CentralDirectoryHeader {
            signature,
            version_made_by,
            version_needed,
            general_purpose_bit_flag,
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
            relative_offset_local_header,
            filename,
            extra_field: extra_field.to_vec(),
            file_comment,
        },
    ))
}

fn parse_end_of_central_directory_record(
    i: &[u8],
) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (i, (signature, disk_number, disk_number_with_cd, num_entries_on_disk, num_entries_total, size_of_cd, offset_of_cd, comment_length, comment)) = tuple((
        le_u32,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u32,
        le_u32,
        le_u16,
        map(take, |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned()),
    ))(i)?;
    Ok((
        i,
        EndOfCentralDirectoryRecord {
            signature,
            disk_number,
            disk_number_with_cd,
            num_entries_on_disk,
            num_entries_total,
            size_of_cd,
            offset_of_cd,
            comment_length,
            comment,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    match read_zip_file(filename) {
        Ok(buffer) => {
            match parse_local_file_header(&buffer) {
                Ok((remaining, header)) => {
                    println!("Local File Header: {:?}", header);
                    match parse_central_directory_header(remaining) {
                        Ok((remaining, header)) => {
                            println!("Central Directory Header: {:?}", header);
                            match parse_end_of_central_directory_record(remaining){
                                Ok((_,header)) => println!("End of Central Directory Record: {:?}", header),
                                Err(e) => {
                                    eprintln!("Error parsing end of central directory record: {:?}", e);
                                    process::exit(1);
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("Error parsing central directory header: {:?}", e);
                            process::exit(1);
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Error parsing local file header: {:?}", e);
                    process::exit(1);
                }
            }
        }
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            process::exit(1);
        }
    }
}