// Cargo.toml
//
// [package]
// name = "zip_parser"
// version = "0.1.0"
// edition = "2021"
//
// [dependencies]
// nom = "7.1"
// clap = { version = "4.0", features = ["derive"] }

use clap::Parser;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt, verify},
    multi::many_till,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input binary file
    #[arg(short, long)]
    input: PathBuf,
}

#[derive(Debug)]
struct LocalFileHeader {
    version: u16,
    flags: u16,
    compression: u16,
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

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], LocalFileHeader> {
    let (input, _) = tag([0x50, 0x4B, 0x03, 0x04])(input)?;
    let (input, (version, flags, compression, mod_time, mod_date, crc32, compressed_size, uncompressed_size, file_name_length, extra_field_length)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u32, le_u16, le_u16))(input)?;
    let (input, file_name) = take(file_name_length)(input)?;
    let (input, extra_field) = take(extra_field_length)(input)?;

    Ok((
        input,
        LocalFileHeader {
            version,
            flags,
            compression,
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

#[derive(Debug)]
struct EndOfCentralDirRecord {
    disk_number: u16,
    central_dir_start_disk: u16,
    num_central_dir_records_on_disk: u16,
    total_central_dir_records: u16,
    central_dir_size: u32,
    central_dir_offset: u32,
    comment_length: u16,
    comment: Vec<u8>,
}

fn parse_end_of_central_dir_record(input: &[u8]) -> IResult<&[u8], EndOfCentralDirRecord> {
    let (input, _) = tag([0x50, 0x4B, 0x05, 0x06])(input)?;
    let (input, (disk_number, central_dir_start_disk, num_central_dir_records_on_disk, total_central_dir_records, central_dir_size, central_dir_offset, comment_length)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u32, le_u32, le_u16))(input)?;
    let (input, comment) = take(comment_length)(input)?;

    Ok((
        input,
        EndOfCentralDirRecord {
            disk_number,
            central_dir_start_disk,
            num_central_dir_records_on_disk,
            total_central_dir_records,
            central_dir_size,
            central_dir_offset,
            comment_length,
            comment: comment.to_vec(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    let mut file = File::open(args.input)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match many_till(parse_local_file_header, parse_end_of_central_dir_record)(&buffer) {
        Ok((_, (headers, eocd))) => {
            println!("Parsed ZIP file successfully!");
            for (i, header) in headers.iter().enumerate() {
                println!("Local File Header {}: {:?}", i + 1, header);
            }
            println!("End of Central Directory Record: {:?}", eocd);
        }
        Err(e) => {
            eprintln!("Failed to parse ZIP file: {:?}", e);
        }
    }

    Ok(())
}