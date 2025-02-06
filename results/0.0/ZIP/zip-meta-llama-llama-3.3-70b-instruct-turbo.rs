use nom::{
    bytes::complete::{take},
    combinator::{map, map_res},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, Result};

#[derive(Debug)]
enum CompressionMethod {
    Stored,
    Shrunk,
    Imploded,
    Tokenized,
    Deflated,
    Deflated64,
    Imploded64,
    Tokenized64,
    Deflated64Bzip2,
    AesEncrypted,
    PpmhHuffman,
    Lzma,
    Lzma2,
    Ppmh,
    Bzip2,
    IbmTerse,
    Lz77,
    Wavpack,
    PpmhHuffmanLzma2,
}

impl CompressionMethod {
    fn from_u16(n: u16) -> CompressionMethod {
        match n {
            0 => CompressionMethod::Stored,
            1 => CompressionMethod::Shrunk,
            2 => CompressionMethod::Imploded,
            3 => CompressionMethod::Tokenized,
            4 => CompressionMethod::Deflated,
            5 => CompressionMethod::Deflated64,
            6 => CompressionMethod::Imploded64,
            7 => CompressionMethod::Tokenized64,
            8 => CompressionMethod::Deflated64Bzip2,
            9 => CompressionMethod::AesEncrypted,
            10 => CompressionMethod::PpmhHuffman,
            11 => CompressionMethod::Lzma,
            12 => CompressionMethod::Lzma2,
            13 => CompressionMethod::Ppmh,
            14 => CompressionMethod::Bzip2,
            15 => panic!("Reserved"),
            16 => CompressionMethod::IbmTerse,
            17 => CompressionMethod::Lz77,
            18 => CompressionMethod::Wavpack,
            19 => CompressionMethod::PpmhHuffmanLzma2,
            _ => panic!("Unknown compression method"),
        }
    }
}

#[derive(Debug)]
struct LocalFileHeader {
    signature: u32,
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: CompressionMethod,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
}

impl LocalFileHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
        let (input, signature) = map_res(take(4usize), |x: &[u8]| {
            Ok(u32::from_be_bytes(x.try_into().unwrap()))
        })(input)?;
        let (input, version_needed_to_extract) = be_u16(input)?;
        let (input, general_purpose_bit_flag) = be_u16(input)?;
        let (input, compression_method) = map(be_u16, CompressionMethod::from_u16)(input)?;
        let (input, last_mod_time) = be_u16(input)?;
        let (input, last_mod_date) = be_u16(input)?;
        let (input, crc32) = be_u32(input)?;
        let (input, compressed_size) = be_u32(input)?;
        let (input, uncompressed_size) = be_u32(input)?;
        let (input, filename_length) = be_u16(input)?;
        let (input, extra_field_length) = be_u16(input)?;
        Ok((
            input,
            LocalFileHeader {
                signature,
                version_needed_to_extract,
                general_purpose_bit_flag,
                compression_method,
                last_mod_time,
                last_mod_date,
                crc32,
                compressed_size,
                uncompressed_size,
                filename_length,
                extra_field_length,
            },
        ))
    }
}

#[derive(Debug)]
struct DataDescriptor {
    signature: u32,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
}

impl DataDescriptor {
    fn parse(input: &[u8]) -> IResult<&[u8], DataDescriptor> {
        let (input, signature) = map_res(take(4usize), |x: &[u8]| {
            Ok(u32::from_be_bytes(x.try_into().unwrap()))
        })(input)?;
        let (input, crc32) = be_u32(input)?;
        let (input, compressed_size) = be_u32(input)?;
        let (input, uncompressed_size) = be_u32(input)?;
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
}

#[derive(Debug)]
struct CentralDirectoryHeader {
    signature: u32,
    version_made_by: u16,
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: CompressionMethod,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    filename_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    internal_attributes: u16,
    external_attributes: u32,
    local_header_offset: u32,
}

impl CentralDirectoryHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], CentralDirectoryHeader> {
        let (input, signature) = map_res(take(4usize), |x: &[u8]| {
            Ok(u32::from_be_bytes(x.try_into().unwrap()))
        })(input)?;
        let (input, version_made_by) = be_u16(input)?;
        let (input, version_needed_to_extract) = be_u16(input)?;
        let (input, general_purpose_bit_flag) = be_u16(input)?;
        let (input, compression_method) = map(be_u16, CompressionMethod::from_u16)(input)?;
        let (input, last_mod_time) = be_u16(input)?;
        let (input, last_mod_date) = be_u16(input)?;
        let (input, crc32) = be_u32(input)?;
        let (input, compressed_size) = be_u32(input)?;
        let (input, uncompressed_size) = be_u32(input)?;
        let (input, filename_length) = be_u16(input)?;
        let (input, extra_field_length) = be_u16(input)?;
        let (input, file_comment_length) = be_u16(input)?;
        let (input, disk_number_start) = be_u16(input)?;
        let (input, internal_attributes) = be_u16(input)?;
        let (input, external_attributes) = be_u32(input)?;
        let (input, local_header_offset) = be_u32(input)?;
        Ok((
            input,
            CentralDirectoryHeader {
                signature,
                version_made_by,
                version_needed_to_extract,
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
                internal_attributes,
                external_attributes,
                local_header_offset,
            },
        ))
    }
}

#[derive(Debug)]
struct EndOfCentralDirectory {
    signature: u32,
    number_of_this_disk: u16,
    number_of_the_disk_where_the_central_directory_starts: u16,
    number_of_entries_in_the_central_directory_on_this_disk: u16,
    number_of_entries_in_the_central_directory: u16,
    size_of_the_central_directory: u32,
    offset_of_start_of_central_directory: u32,
    zipfile_comment_length: u16,
}

impl EndOfCentralDirectory {
    fn parse(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectory> {
        let (input, signature) = map_res(take(4usize), |x: &[u8]| {
            Ok(u32::from_be_bytes(x.try_into().unwrap()))
        })(input)?;
        let (input, number_of_this_disk) = be_u16(input)?;
        let (input, number_of_the_disk_where_the_central_directory_starts) = be_u16(input)?;
        let (input, number_of_entries_in_the_central_directory_on_this_disk) = be_u16(input)?;
        let (input, number_of_entries_in_the_central_directory) = be_u16(input)?;
        let (input, size_of_the_central_directory) = be_u32(input)?;
        let (input, offset_of_start_of_central_directory) = be_u32(input)?;
        let (input, zipfile_comment_length) = be_u16(input)?;
        Ok((
            input,
            EndOfCentralDirectory {
                signature,
                number_of_this_disk,
                number_of_the_disk_where_the_central_directory_starts,
                number_of_entries_in_the_central_directory_on_this_disk,
                number_of_entries_in_the_central_directory,
                size_of_the_central_directory,
                offset_of_start_of_central_directory,
                zipfile_comment_length,
            },
        ))
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <zip_file>", args[0]);
        return Ok(());
    }
    let mut file = File::open(&args[1])?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    let (_input, local_file_header) = LocalFileHeader::parse(&data).unwrap();
    println!("Local File Header: {:?}", local_file_header);
    let (_input, data_descriptor) = DataDescriptor::parse(&data).unwrap();
    println!("Data Descriptor: {:?}", data_descriptor);
    let (_input, central_directory_header) = CentralDirectoryHeader::parse(&data).unwrap();
    println!("Central Directory Header: {:?}", central_directory_header);
    let (_input, end_of_central_directory) = EndOfCentralDirectory::parse(&data).unwrap();
    println!("End of Central Directory: {:?}", end_of_central_directory);
    Ok(())
}