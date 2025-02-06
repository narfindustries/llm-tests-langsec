use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{many0, many_m_n},
    number::complete::{be_u16, be_u32, be_u64},
    IResult,
};
use std::{
    collections::HashMap,
    fs::File,
    io::{BufReader, Read},
};
use std::path::Path;

#[derive(Debug)]
enum CompressionMethod {
    Stored,
    Shrunk,
    Reduced(u8),
    Imploded,
    Tokenized,
    Deflated,
    Deflated64,
    ImplodedPlus,
    Wave,
    PPMd,
}

impl CompressionMethod {
    fn from_u16(n: u16) -> CompressionMethod {
        match n {
            0 => CompressionMethod::Stored,
            1 => CompressionMethod::Shrunk,
            2 => CompressionMethod::Reduced(1),
            3 => CompressionMethod::Reduced(2),
            4 => CompressionMethod::Reduced(3),
            5 => CompressionMethod::Reduced(4),
            6 => CompressionMethod::Imploded,
            7 => CompressionMethod::Tokenized,
            8 => CompressionMethod::Deflated,
            9 => CompressionMethod::Deflated64,
            10 => CompressionMethod::ImplodedPlus,
            11 => CompressionMethod::Wave,
            12 => CompressionMethod::PPMd,
            _ => panic!("Invalid compression method"),
        }
    }
}

#[derive(Debug)]
struct LocalFileHeader {
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: CompressionMethod,
    last_modification_time: u16,
    last_modification_date: u16,
    crc_32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    filename: Vec<u8>,
    extra_field: Vec<u8>,
}

fn local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, signature) = tag(&[0x50, 0x4b, 0x03, 0x04])(input)?;
    let (input, version_needed_to_extract) = be_u16(input)?;
    let (input, general_purpose_bit_flag) = be_u16(input)?;
    let (input, compression_method) = map(be_u16, CompressionMethod::from_u16)(input)?;
    let (input, last_modification_time) = be_u16(input)?;
    let (input, last_modification_date) = be_u16(input)?;
    let (input, crc_32) = be_u32(input)?;
    let (input, compressed_size) = be_u32(input)?;
    let (input, uncompressed_size) = be_u32(input)?;
    let (input, filename_length) = be_u16(input)?;
    let (input, extra_field_length) = be_u16(input)?;
    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    Ok((
        input,
        LocalFileHeader {
            version_needed_to_extract,
            general_purpose_bit_flag,
            compression_method,
            last_modification_time,
            last_modification_date,
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

#[derive(Debug)]
struct CentralDirectory {
    version_made_by: u16,
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: CompressionMethod,
    last_modification_time: u16,
    last_modification_date: u16,
    crc_32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    internal_file_attributes: u16,
    external_file_attributes: u32,
    local_header_offset: u32,
    filename: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

fn central_directory(input: &[u8]) -> IResult<&[u8], CentralDirectory> {
    let (input, signature) = tag(&[0x50, 0x4b, 0x01, 0x02])(input)?;
    let (input, version_made_by) = be_u16(input)?;
    let (input, version_needed_to_extract) = be_u16(input)?;
    let (input, general_purpose_bit_flag) = be_u16(input)?;
    let (input, compression_method) = map(be_u16, CompressionMethod::from_u16)(input)?;
    let (input, last_modification_time) = be_u16(input)?;
    let (input, last_modification_date) = be_u16(input)?;
    let (input, crc_32) = be_u32(input)?;
    let (input, compressed_size) = be_u32(input)?;
    let (input, uncompressed_size) = be_u32(input)?;
    let (input, filename_length) = be_u16(input)?;
    let (input, extra_field_length) = be_u16(input)?;
    let (input, file_comment_length) = be_u16(input)?;
    let (input, disk_number_start) = be_u16(input)?;
    let (input, internal_file_attributes) = be_u16(input)?;
    let (input, external_file_attributes) = be_u32(input)?;
    let (input, local_header_offset) = be_u32(input)?;
    let (input, filename) = take(filename_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;
    let (input, file_comment) = take(file_comment_length)(input)?;
    Ok((
        input,
        CentralDirectory {
            version_made_by,
            version_needed_to_extract,
            general_purpose_bit_flag,
            compression_method,
            last_modification_time,
            last_modification_date,
            crc_32,
            compressed_size,
            uncompressed_size,
            filename_length,
            extra_field_length,
            file_comment_length,
            disk_number_start,
            internal_file_attributes,
            external_file_attributes,
            local_header_offset,
            filename: filename.to_vec(),
            extra_field: extra_field.to_vec(),
            file_comment: file_comment.to_vec(),
        },
    ))
}

#[derive(Debug)]
struct EndOfCentralDirectory {
    number_of_this_disk: u16,
    number_of_the_disk_where_the_central_directory_starts: u16,
    number_of_entries_in_the_central_directory_on_this_disk: u16,
    number_of_entries_in_the_central_directory: u16,
    size_of_the_central_directory: u32,
    offset_of_the_start_of_the_central_directory: u32,
    comment_length: u16,
    comment: Vec<u8>,
}

fn end_of_central_directory(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectory> {
    let (input, signature) = tag(&[0x50, 0x4b, 0x05, 0x06])(input)?;
    let (input, number_of_this_disk) = be_u16(input)?;
    let (input, number_of_the_disk_where_the_central_directory_starts) = be_u16(input)?;
    let (input, number_of_entries_in_the_central_directory_on_this_disk) = be_u16(input)?;
    let (input, number_of_entries_in_the_central_directory) = be_u16(input)?;
    let (input, size_of_the_central_directory) = be_u32(input)?;
    let (input, offset_of_the_start_of_the_central_directory) = be_u32(input)?;
    let (input, comment_length) = be_u16(input)?;
    let (input, comment) = take(comment_length)(input)?;
    Ok((
        input,
        EndOfCentralDirectory {
            number_of_this_disk,
            number_of_the_disk_where_the_central_directory_starts,
            number_of_entries_in_the_central_directory_on_this_disk,
            number_of_entries_in_the_central_directory,
            size_of_the_central_directory,
            offset_of_the_start_of_the_central_directory,
            comment_length,
            comment: comment.to_vec(),
        },
    ))
}

#[derive(Debug)]
enum ZipFile {
    LocalFileHeader(LocalFileHeader),
    CentralDirectory(CentralDirectory),
    EndOfCentralDirectory(EndOfCentralDirectory),
}

fn zip_file(input: &[u8]) -> IResult<&[u8], ZipFile> {
    let (input, signature) = tag(&[0x50, 0x4b, 0x03, 0x04])(input)?;
    let (input, local_file_header) = local_file_header(input)?;
    Ok((input, ZipFile::LocalFileHeader(local_file_header)))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let file = File::open(input_file).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let result = zip_file(&input);
    match result {
        Ok((remaining, zip_file)) => {
            println!("{:?}", zip_file);
        }
        Err(err) => {
            println!("Error: {}", err);
        }
    }
}