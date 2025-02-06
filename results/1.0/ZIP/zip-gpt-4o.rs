use nom::bytes::complete::{tag, take};
use nom::number::complete::{le_u16, le_u32};
use nom::IResult;
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct ZipHeader {
    version_needed: u16,
    general_purpose_bit_flag: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
}

fn parse_local_file_header(input: &[u8]) -> IResult<&[u8], ZipHeader> {
    let (input, _) = tag([0x50, 0x4b, 0x03, 0x04])(input)?; // Local file header signature
    let (input, version_needed) = le_u16(input)?;
    let (input, general_purpose_bit_flag) = le_u16(input)?;
    let (input, compression_method) = le_u16(input)?;
    let (input, last_mod_time) = le_u16(input)?;
    let (input, last_mod_date) = le_u16(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, compressed_size) = le_u32(input)?;
    let (input, uncompressed_size) = le_u32(input)?;
    let (input, file_name_length) = le_u16(input)?;
    let (input, extra_field_length) = le_u16(input)?;
    
    Ok((input, ZipHeader {
        version_needed,
        general_purpose_bit_flag,
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        file_name_length,
        extra_field_length,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let mut file = File::open(file_path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let mut input = buffer.as_slice();
    loop {
        match parse_local_file_header(input) {
            Ok((remaining_input, header)) => {
                println!("{:?}", header);

                // Skip file name and extra field to move to next header
                let (remaining_input, _) = take::<_, _, nom::error::Error<_>>(header.file_name_length as usize)(remaining_input).unwrap();
                let (remaining_input, _) = take::<_, _, nom::error::Error<_>>(header.extra_field_length as usize)(remaining_input).unwrap();
                
                // Move to file data based on compressed_size
                let (remaining_input, _) = take::<_, _, nom::error::Error<_>>(header.compressed_size as usize)(remaining_input).unwrap();

                input = remaining_input;

                if input.is_empty() {
                    break;
                }
            }
            Err(_) => break,
        }
    }
}