use nom::{
    bytes::complete::{take},
    combinator::{map_res},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
enum CompressionMethod {
    Stored,
    Shrunk,
    Imploded,
    Tokenized,
    Deflated,
    ImplodedDeflated,
    BZip2,
    Lzma,
    Terse,
    Lz77,
    WavPack,
    PpmD,
    Aes128,
    Aes192,
    Aes256,
}

#[derive(Debug)]
struct LocalHeader {
    local_header_signature: u32,
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: CompressionMethod,
    last_mod_time: u16,
    last_mod_date: u16,
    crc_32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    filename: Vec<u8>,
    extra_field: Vec<u8>,
}

#[derive(Debug)]
struct CentralDirectoryHeader {
    central_header_signature: u32,
    version_made_by: u16,
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: CompressionMethod,
    last_mod_time: u16,
    last_mod_date: u16,
    crc_32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    int_attributes: u16,
    ext_attributes: u32,
    local_header_offset: u32,
    filename: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    end_of_central_dir_signature: u32,
    number_of_this_disk: u16,
    number_of_the_disk_with_the_start_of_the_central_directory: u16,
    total_number_of_entries_in_the_central_directory_on_this_disk: u16,
    total_number_of_entries_in_the_central_directory: u16,
    size_of_the_central_directory: u32,
    offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number: u32,
    zipfile_comment_length: u16,
    zipfile_comment: Vec<u8>,
}

fn parse_local_header(input: &[u8]) -> IResult<&[u8], LocalHeader> {
    let (input, local_header_signature) = map_res(take(4usize), |x: &[u8]| {
        Ok(u32::from_be_bytes(x.try_into().unwrap()))
    })(input)?;
    assert_eq!(local_header_signature, 0x04034b50);
    let (input, version_needed_to_extract) = be_u16(input)?;
    let (input, general_purpose_bit_flag) = be_u16(input)?;
    let (input, compression_method) = map_res(be_u16, |x: u16| match x {
        0 => Ok(CompressionMethod::Stored),
        1 => Ok(CompressionMethod::Shrunk),
        2 => Ok(CompressionMethod::Imploded),
        3 => Ok(CompressionMethod::Tokenized),
        4 => Ok(CompressionMethod::Deflated),
        5 => Ok(CompressionMethod::ImplodedDeflated),
        6 => Ok(CompressionMethod::BZip2),
        7 => Ok(CompressionMethod::Lzma),
        8 => Ok(CompressionMethod::Terse),
        9 => Ok(CompressionMethod::Lz77),
        10 => Ok(CompressionMethod::WavPack),
        11 => Ok(CompressionMethod::PpmD),
        12 => Ok(CompressionMethod::Aes128),
        13 => Ok(CompressionMethod::Aes192),
        14 => Ok(CompressionMethod::Aes256),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::Fail)),
    })(input)?;
    let (input, last_mod_time) = be_u16(input)?;
    let (input, last_mod_date) = be_u16(input)?;
    let (input, crc_32) = be_u32(input)?;
    let (input, compressed_size) = be_u32(input)?;
    let (input, uncompressed_size) = be_u32(input)?;
    let (input, filename_length) = be_u16(input)?;
    let (input, extra_field_length) = be_u16(input)?;
    let (input, filename) = take(filename_length as usize)(input)?;
    let (input, extra_field) = take(extra_field_length as usize)(input)?;
    Ok((
        input,
        LocalHeader {
            local_header_signature,
            version_needed_to_extract,
            general_purpose_bit_flag,
            compression_method,
            last_mod_time,
            last_mod_date,
            crc_32,
            compressed_size,
            uncompressed_size,
            filename_length,
            extra_field_length,
            filename: filename.to_vec(),
            extra_field: extra_field.to_vec(),
        },
    ))
}

fn parse_central_directory_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryHeader> {
    let (input, central_header_signature) = map_res(take(4usize), |x: &[u8]| {
        Ok(u32::from_be_bytes(x.try_into().unwrap()))
    })(input)?;
    assert_eq!(central_header_signature, 0x02014b50);
    let (input, version_made_by) = be_u16(input)?;
    let (input, version_needed_to_extract) = be_u16(input)?;
    let (input, general_purpose_bit_flag) = be_u16(input)?;
    let (input, compression_method) = map_res(be_u16, |x: u16| match x {
        0 => Ok(CompressionMethod::Stored),
        1 => Ok(CompressionMethod::Shrunk),
        2 => Ok(CompressionMethod::Imploded),
        3 => Ok(CompressionMethod::Tokenized),
        4 => Ok(CompressionMethod::Deflated),
        5 => Ok(CompressionMethod::ImplodedDeflated),
        6 => Ok(CompressionMethod::BZip2),
        7 => Ok(CompressionMethod::Lzma),
        8 => Ok(CompressionMethod::Terse),
        9 => Ok(CompressionMethod::Lz77),
        10 => Ok(CompressionMethod::WavPack),
        11 => Ok(CompressionMethod::PpmD),
        12 => Ok(CompressionMethod::Aes128),
        13 => Ok(CompressionMethod::Aes192),
        14 => Ok(CompressionMethod::Aes256),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::Fail)),
    })(input)?;
    let (input, last_mod_time) = be_u16(input)?;
    let (input, last_mod_date) = be_u16(input)?;
    let (input, crc_32) = be_u32(input)?;
    let (input, compressed_size) = be_u32(input)?;
    let (input, uncompressed_size) = be_u32(input)?;
    let (input, filename_length) = be_u16(input)?;
    let (input, extra_field_length) = be_u16(input)?;
    let (input, file_comment_length) = be_u16(input)?;
    let (input, disk_number_start) = be_u16(input)?;
    let (input, int_attributes) = be_u16(input)?;
    let (input, ext_attributes) = be_u32(input)?;
    let (input, local_header_offset) = be_u32(input)?;
    let (input, filename) = take(filename_length as usize)(input)?;
    let (input, extra_field) = take(extra_field_length as usize)(input)?;
    let (input, file_comment) = take(file_comment_length as usize)(input)?;
    Ok((
        input,
        CentralDirectoryHeader {
            central_header_signature,
            version_made_by,
            version_needed_to_extract,
            general_purpose_bit_flag,
            compression_method,
            last_mod_time,
            last_mod_date,
            crc_32,
            compressed_size,
            uncompressed_size,
            filename_length,
            extra_field_length,
            file_comment_length,
            disk_number_start,
            int_attributes,
            ext_attributes,
            local_header_offset,
            filename: filename.to_vec(),
            extra_field: extra_field.to_vec(),
            file_comment: file_comment.to_vec(),
        },
    ))
}

fn parse_end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (input, end_of_central_dir_signature) = map_res(take(4usize), |x: &[u8]| {
        Ok(u32::from_be_bytes(x.try_into().unwrap()))
    })(input)?;
    assert_eq!(end_of_central_dir_signature, 0x06054b50);
    let (input, number_of_this_disk) = be_u16(input)?;
    let (input, number_of_the_disk_with_the_start_of_the_central_directory) = be_u16(input)?;
    let (input, total_number_of_entries_in_the_central_directory_on_this_disk) = be_u16(input)?;
    let (input, total_number_of_entries_in_the_central_directory) = be_u16(input)?;
    let (input, size_of_the_central_directory) = be_u32(input)?;
    let (input, offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number) = be_u32(input)?;
    let (input, zipfile_comment_length) = be_u16(input)?;
    let (input, zipfile_comment) = take(zipfile_comment_length as usize)(input)?;
    Ok((
        input,
        EndOfCentralDirectoryRecord {
            end_of_central_dir_signature,
            number_of_this_disk,
            number_of_the_disk_with_the_start_of_the_central_directory,
            total_number_of_entries_in_the_central_directory_on_this_disk,
            total_number_of_entries_in_the_central_directory,
            size_of_the_central_directory,
            offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number,
            zipfile_comment_length,
            zipfile_comment: zipfile_comment.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (input, local_header) = parse_local_header(&input).unwrap();
    println!("Local Header: {:?}", local_header);
    let (input, central_directory_header) = parse_central_directory_header(input).unwrap();
    println!("Central Directory Header: {:?}", central_directory_header);
    let (_input, end_of_central_directory_record) = parse_end_of_central_directory_record(input).unwrap();
    println!("End of Central Directory Record: {:?}", end_of_central_directory_record);
}