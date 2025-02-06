use nom::branch::alt;
use nom::bytes::complete::{take};
use nom::combinator::{map};
use nom::error::{ErrorKind, ParseError};
use nom::number::complete::{be_u16, be_u32};
use nom::sequence::{tuple};
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug, PartialEq)]
enum ZipError {
    InvalidSignature,
    InvalidCompressionMethod,
    InvalidVersion,
    UnexpectedEof,
}

impl std::error::Error for ZipError {}

impl std::fmt::Display for ZipError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ZipError::InvalidSignature => write!(f, "Invalid signature"),
            ZipError::InvalidCompressionMethod => write!(f, "Invalid compression method"),
            ZipError::InvalidVersion => write!(f, "Invalid version"),
            ZipError::UnexpectedEof => write!(f, "Unexpected end of file"),
        }
    }
}

impl ParseError<&[u8]> for ZipError {
    fn from_error_kind(input: &[u8], kind: ErrorKind) -> Self {
        match kind {
            ErrorKind::AlphaNumeric => ZipError::InvalidSignature,
            ErrorKind::Digit => ZipError::InvalidCompressionMethod,
            ErrorKind::Eof => ZipError::UnexpectedEof,
            _ => ZipError::InvalidVersion,
        }
    }

    fn append(input: &[u8], kind: ErrorKind, error: ZipError) -> Self {
        error
    }
}

#[derive(Debug, PartialEq)]
struct LocalFileHeader {
    signature: u32,
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: u16,
    last_modified_time: u16,
    last_modified_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
}

#[derive(Debug, PartialEq)]
struct CentralDirectory {
    signature: u32,
    version_made_by: u16,
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: u16,
    last_modified_time: u16,
    last_modified_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    internal_file_attributes: u16,
    external_file_attributes: u32,
    relative_offset_of_local_header: u32,
}

#[derive(Debug, PartialEq)]
struct DataDescriptor {
    signature: u32,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
}

#[derive(Debug, PartialEq)]
struct EndOfCentralDirectory {
    signature: u32,
    number_of_this_disk: u16,
    number_of_the_disk_where_the_central_directory_starts: u16,
    number_of_entries_in_the_central_directory_on_this_disk: u16,
    total_number_of_entries_in_the_central_directory: u16,
    size_of_the_central_directory: u32,
    offset_of_start_of_central_directory: u32,
    comment_length: u16,
}

fn parse_u32(input: &[u8]) -> IResult<&[u8], u32, ZipError> {
    let (input, bytes) = take(4usize)(input)?;
    let value = u32::from_be_bytes(bytes.try_into().unwrap());
    Ok((input, value))
}

fn parse_u16(input: &[u8]) -> IResult<&[u8], u16, ZipError> {
    let (input, bytes) = take(2usize)(input)?;
    let value = u16::from_be_bytes(bytes.try_into().unwrap());
    Ok((input, value))
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader, ZipError> {
    let (input, signature) = parse_u32(input)?;
    let (input, version_needed_to_extract) = parse_u16(input)?;
    let (input, general_purpose_bit_flag) = parse_u16(input)?;
    let (input, compression_method) = parse_u16(input)?;
    let (input, last_modified_time) = parse_u16(input)?;
    let (input, last_modified_date) = parse_u16(input)?;
    let (input, crc32) = parse_u32(input)?;
    let (input, compressed_size) = parse_u32(input)?;
    let (input, uncompressed_size) = parse_u32(input)?;
    let (input, file_name_length) = parse_u16(input)?;
    let (input, extra_field_length) = parse_u16(input)?;

    Ok((
        input,
        LocalFileHeader {
            signature,
            version_needed_to_extract,
            general_purpose_bit_flag,
            compression_method,
            last_modified_time,
            last_modified_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name_length,
            extra_field_length,
        },
    ))
}

fn parse_central_directory(input: &[u8]) -> IResult<&[u8], CentralDirectory, ZipError> {
    let (input, signature) = parse_u32(input)?;
    let (input, version_made_by) = parse_u16(input)?;
    let (input, version_needed_to_extract) = parse_u16(input)?;
    let (input, general_purpose_bit_flag) = parse_u16(input)?;
    let (input, compression_method) = parse_u16(input)?;
    let (input, last_modified_time) = parse_u16(input)?;
    let (input, last_modified_date) = parse_u16(input)?;
    let (input, crc32) = parse_u32(input)?;
    let (input, compressed_size) = parse_u32(input)?;
    let (input, uncompressed_size) = parse_u32(input)?;
    let (input, file_name_length) = parse_u16(input)?;
    let (input, extra_field_length) = parse_u16(input)?;
    let (input, file_comment_length) = parse_u16(input)?;
    let (input, disk_number_start) = parse_u16(input)?;
    let (input, internal_file_attributes) = parse_u16(input)?;
    let (input, external_file_attributes) = parse_u32(input)?;
    let (input, relative_offset_of_local_header) = parse_u32(input)?;

    Ok((
        input,
        CentralDirectory {
            signature,
            version_made_by,
            version_needed_to_extract,
            general_purpose_bit_flag,
            compression_method,
            last_modified_time,
            last_modified_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name_length,
            extra_field_length,
            file_comment_length,
            disk_number_start,
            internal_file_attributes,
            external_file_attributes,
            relative_offset_of_local_header,
        },
    ))
}

fn parse_data_descriptor(input: &[u8]) -> IResult<&[u8], DataDescriptor, ZipError> {
    let (input, signature) = parse_u32(input)?;
    let (input, crc32) = parse_u32(input)?;
    let (input, compressed_size) = parse_u32(input)?;
    let (input, uncompressed_size) = parse_u32(input)?;

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

fn parse_end_of_central_directory(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectory, ZipError> {
    let (input, signature) = parse_u32(input)?;
    let (input, number_of_this_disk) = parse_u16(input)?;
    let (input, number_of_the_disk_where_the_central_directory_starts) = parse_u16(input)?;
    let (input, number_of_entries_in_the_central_directory_on_this_disk) = parse_u16(input)?;
    let (input, total_number_of_entries_in_the_central_directory) = parse_u16(input)?;
    let (input, size_of_the_central_directory) = parse_u32(input)?;
    let (input, offset_of_start_of_central_directory) = parse_u32(input)?;
    let (input, comment_length) = parse_u16(input)?;

    Ok((
        input,
        EndOfCentralDirectory {
            signature,
            number_of_this_disk,
            number_of_the_disk_where_the_central_directory_starts,
            number_of_entries_in_the_central_directory_on_this_disk,
            total_number_of_entries_in_the_central_directory,
            size_of_the_central_directory,
            offset_of_start_of_central_directory,
            comment_length,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return;
        }
    };

    let mut data = Vec::new();
    match file.read_to_end(&mut data) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    }

    let result = parse_local_file_header(&data);
    match result {
        Ok((_, lfh)) => {
            println!("Local File Header:");
            println!("Signature: 0x{:x}", lfh.signature);
            println!("Version needed to extract: {}", lfh.version_needed_to_extract);
            println!("General purpose bit flag: 0x{:x}", lfh.general_purpose_bit_flag);
            println!("Compression method: {}", lfh.compression_method);
            println!("Last modified time: {}", lfh.last_modified_time);
            println!("Last modified date: {}", lfh.last_modified_date);
            println!("CRC-32: 0x{:x}", lfh.crc32);
            println!("Compressed size: {}", lfh.compressed_size);
            println!("Uncompressed size: {}", lfh.uncompressed_size);
            println!("File name length: {}", lfh.file_name_length);
            println!("Extra field length: {}", lfh.extra_field_length);

            let mut offset = lfh.file_name_length as usize + lfh.extra_field_length as usize;
            while offset < data.len() {
                let result = parse_central_directory(&data[offset..]);
                match result {
                    Ok((_, cd)) => {
                        println!("Central Directory:");
                        println!("Signature: 0x{:x}", cd.signature);
                        println!("Version made by: {}", cd.version_made_by);
                        println!("Version needed to extract: {}", cd.version_needed_to_extract);
                        println!("General purpose bit flag: 0x{:x}", cd.general_purpose_bit_flag);
                        println!("Compression method: {}", cd.compression_method);
                        println!("Last modified time: {}", cd.last_modified_time);
                        println!("Last modified date: {}", cd.last_modified_date);
                        println!("CRC-32: 0x{:x}", cd.crc32);
                        println!("Compressed size: {}", cd.compressed_size);
                        println!("Uncompressed size: {}", cd.uncompressed_size);
                        println!("File name length: {}", cd.file_name_length);
                        println!("Extra field length: {}", cd.extra_field_length);
                        println!("File comment length: {}", cd.file_comment_length);
                        println!("Disk number start: {}", cd.disk_number_start);
                        println!("Internal file attributes: 0x{:x}", cd.internal_file_attributes);
                        println!("External file attributes: 0x{:x}", cd.external_file_attributes);
                        println!("Relative offset of local header: {}", cd.relative_offset_of_local_header);

                        offset += 46;
                    }
                    Err(err) => {
                        println!("Error parsing Central Directory: {:?}", err);
                        break;
                    }
                }
            }

            let result = parse_end_of_central_directory(&data[data.len() - 22..]);
            match result {
                Ok((_, eocd)) => {
                    println!("End of Central Directory:");
                    println!("Signature: 0x{:x}", eocd.signature);
                    println!("Number of this disk: {}", eocd.number_of_this_disk);
                    println!("Number of the disk where the central directory starts: {}", eocd.number_of_the_disk_where_the_central_directory_starts);
                    println!("Number of entries in the central directory on this disk: {}", eocd.number_of_entries_in_the_central_directory_on_this_disk);
                    println!("Total number of entries in the central directory: {}", eocd.total_number_of_entries_in_the_central_directory);
                    println!("Size of the central directory: {}", eocd.size_of_the_central_directory);
                    println!("Offset of start of central directory: {}", eocd.offset_of_start_of_central_directory);
                    println!("Comment length: {}", eocd.comment_length);
                }
                Err(err) => {
                    println!("Error parsing End of Central Directory: {:?}", err);
                }
            }
        }
        Err(err) => {
            println!("Error parsing Local File Header: {:?}", err);
        }
    }
}