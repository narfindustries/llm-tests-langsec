use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt, verify},
    multi::{many_till, take_while_m_n},
    sequence::{pair, tuple},
    IResult,
};
use std::{
    fs::File,
    io::{BufReader, Read},
    path::Path,
};
use std::str;

const LOCAL_FILE_HEADER: &[u8] = &[0x50, 0x4b, 0x03, 0x04];
const CENTRAL_DIRECTORY_HEADER: &[u8] = &[0x50, 0x4b, 0x01, 0x02];
const END_OF_CENTRAL_DIRECTORY: &[u8] = &[0x50, 0x4b, 0x05, 0x06];

#[derive(Debug, PartialEq)]
enum CompressionMethod {
    Stored,
    Shrunk,
    Reduced1,
    Reduced2,
    Reduced3,
    Reduced4,
    Imploded,
    Tokenized,
    Deflated,
    EnhancedDeflated,
    PKWareImploded,
    BZip2,
    Reserved,
    Invalid,
}

impl CompressionMethod {
    fn from_u16(val: u16) -> CompressionMethod {
        match val {
            0 => CompressionMethod::Stored,
            1 => CompressionMethod::Shrunk,
            2 => CompressionMethod::Reduced1,
            3 => CompressionMethod::Reduced2,
            4 => CompressionMethod::Reduced3,
            5 => CompressionMethod::Reduced4,
            6 => CompressionMethod::Imploded,
            7 => CompressionMethod::Tokenized,
            8 => CompressionMethod::Deflated,
            9 => CompressionMethod::EnhancedDeflated,
            10 => CompressionMethod::PKWareImploded,
            12 => CompressionMethod::BZip2,
            _ => CompressionMethod::Invalid,
        }
    }
}

#[derive(Debug, PartialEq)]
enum GeneralPurposeBitFlag {
    Encrypted,
    CompressionOptions,
    DataDescriptor,
    EnhancedDeflation,
    CompressedPatchedData,
    StrongEncryption,
}

impl GeneralPurposeBitFlag {
    fn from_u16(val: u16) -> Vec<GeneralPurposeBitFlag> {
        let mut flags = Vec::new();
        if (val & 0x0001) != 0 {
            flags.push(GeneralPurposeBitFlag::Encrypted);
        }
        if (val & 0x0002) != 0 {
            flags.push(GeneralPurposeBitFlag::CompressionOptions);
        }
        if (val & 0x0004) != 0 {
            flags.push(GeneralPurposeBitFlag::DataDescriptor);
        }
        if (val & 0x0008) != 0 {
            flags.push(GeneralPurposeBitFlag::EnhancedDeflation);
        }
        if (val & 0x0010) != 0 {
            flags.push(GeneralPurposeBitFlag::CompressedPatchedData);
        }
        if (val & 0x0040) != 0 {
            flags.push(GeneralPurposeBitFlag::StrongEncryption);
        }
        flags
    }
}

#[derive(Debug, PartialEq)]
struct LocalFileHeader {
    version_needed_to_extract: u16,
    general_purpose_bit_flag: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc_32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_name: String,
    extra_field: Vec<u8>,
}

impl LocalFileHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
        let (input, _) = tag(LOCAL_FILE_HEADER)(input)?;
        let (input, version_needed_to_extract) = map(take(2usize), |b: &[u8]| u16::from_le_bytes([b[0], b[1]]))(
            input,
        )?;
        let (input, general_purpose_bit_flag) =
            map(take(2usize), |b: &[u8]| u16::from_le_bytes([b[0], b[1]]))(input)?;
        let (input, compression_method) =
            map(take(2usize), |b: &[u8]| u16::from_le_bytes([b[0], b[1]]))(input)?;
        let (input, last_mod_time) = map(take(2usize), |b: &[u8]| u16::from_le_bytes([b[0], b[1]]))(input)?;
        let (input, last_mod_date) = map(take(2usize), |b: &[u8]| u16::from_le_bytes([b[0], b[1]]))(input)?;
        let (input, crc_32) = map(take(4usize), |b: &[u8]| u32::from_le_bytes([b[0], b[1], b[2], b[3]]))(input)?;
        let (input, compressed_size) =
            map(take(4usize), |b: &[u8]| u32::from_le_bytes([b[0], b[1], b[2], b[3]]))(input)?;
        let (input, uncompressed_size) =
            map(take(4usize), |b: &[u8]| u32::from_le_bytes([b[0], b[1], b[2], b[3]]))(input)?;
        let (input, file_name_length) = map(take(2usize), |b: &[u8]| u16::from_le_bytes([b[0], b[1]]))(
            input,
        )?;
        let (input, extra_field_length) =
            map(take(2usize), |b: &[u8]| u16::from_le_bytes([b[0], b[1]]))(input)?;
        let (input, file_name) = map(take(file_name_length as usize), |b: &[u8]| {
            String::from_utf8_lossy(b).into_owned()
        })(input)?;
        let (input, extra_field) = take(extra_field_length as usize)(input)?;
        Ok((
            input,
            LocalFileHeader {
                version_needed_to_extract,
                general_purpose_bit_flag,
                compression_method,
                last_mod_time,
                last_mod_date,
                crc_32,
                compressed_size,
                uncompressed_size,
                file_name_length,
                extra_field_length,
                file_name,
                extra_field: extra_field.to_vec(),
            },
        ))
    }
}

#[derive(Debug, PartialEq)]
struct EndOfCentralDirectory {
    disk_number: u16,
    central_directory_disk_number: u16,
    num_entries_this_disk: u16,
    num_entries_total: u16,
    central_directory_size: u32,
    central_directory_offset: u32,
    comment_length: u16,
    comment: String,
}

impl EndOfCentralDirectory {
    fn parse(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectory> {
        let (input, _) = tag(END_OF_CENTRAL_DIRECTORY)(input)?;
        let (input, disk_number) = map(take(2usize), |b: &[u8]| u16::from_le_bytes([b[0], b[1]]))(
            input,
        )?;
        let (input, central_directory_disk_number) =
            map(take(2usize), |b: &[u8]| u16::from_le_bytes([b[0], b[1]]))(input)?;
        let (input, num_entries_this_disk) =
            map(take(2usize), |b: &[u8]| u16::from_le_bytes([b[0], b[1]]))(input)?;
        let (input, num_entries_total) =
            map(take(2usize), |b: &[u8]| u16::from_le_bytes([b[0], b[1]]))(input)?;
        let (input, central_directory_size) =
            map(take(4usize), |b: &[u8]| u32::from_le_bytes([b[0], b[1], b[2], b[3]]))(input)?;
        let (input, central_directory_offset) =
            map(take(4usize), |b: &[u8]| u32::from_le_bytes([b[0], b[1], b[2], b[3]]))(input)?;
        let (input, comment_length) = map(take(2usize), |b: &[u8]| u16::from_le_bytes([b[0], b[1]]))(
            input,
        )?;
        let (input, comment) = map(take(comment_length as usize), |b: &[u8]| {
            String::from_utf8_lossy(b).into_owned()
        })(input)?;
        Ok((
            input,
            EndOfCentralDirectory {
                disk_number,
                central_directory_disk_number,
                num_entries_this_disk,
                num_entries_total,
                central_directory_size,
                central_directory_offset,
                comment_length,
                comment,
            },
        ))
    }
}

fn main() -> std::io::Result<()> {
    let path = std::env::args().nth(1).expect("missing file path");
    let file = File::open(Path::new(&path))?;
    let reader = BufReader::new(file);
    let mut data = Vec::new();
    reader.read_to_end(&mut data)?;
    let _ = LocalFileHeader::parse(&data);
    let _ = EndOfCentralDirectory::parse(&data);
    Ok(())
}