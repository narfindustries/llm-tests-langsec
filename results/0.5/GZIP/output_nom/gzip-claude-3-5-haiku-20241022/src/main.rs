use nom::{
    bytes::complete::{tag, take, take_while},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::str;

#[derive(Debug)]
struct GzipHeader {
    identification: [u8; 2],
    compression_method: u8,
    flags: u8,
    modification_time: u32,
    extra_flags: u8,
    operating_system: u8,
    extra_fields: Option<Vec<u8>>,
    original_filename: Option<String>,
    file_comment: Option<String>,
    header_crc16: Option<u16>,
}

#[derive(Debug)]
struct GzipTrailer {
    crc32: u32,
    original_length: u32,
}

#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    compressed_data: Vec<u8>,
    trailer: GzipTrailer,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, identification) = tag(&[0x1F, 0x8B])(input)?;
    let (input, compression_method) = le_u8(input)?;
    let (input, flags) = le_u8(input)?;
    let (input, modification_time) = le_u32(input)?;
    let (input, extra_flags) = le_u8(input)?;
    let (input, operating_system) = le_u8(input)?;

    let (input, extra_fields) = if flags & 0x04 != 0 {
        let (input, extra_length) = le_u16(input)?;
        let (input, extra_data) = take(extra_length as usize)(input)?;
        (input, Some(extra_data.to_vec()))
    } else {
        (input, None)
    };

    let (input, original_filename) = if flags & 0x08 != 0 {
        let (input, filename) = take_while(|c| c != 0)(input)?;
        let filename_str = str::from_utf8(filename).map(|s| s.to_string()).unwrap_or_default();
        let input = &input[1..];
        (input, Some(filename_str))
    } else {
        (input, None)
    };

    let (input, file_comment) = if flags & 0x10 != 0 {
        let (input, comment) = take_while(|c| c != 0)(input)?;
        let comment_str = str::from_utf8(comment).map(|s| s.to_string()).unwrap_or_default();
        let input = &input[1..];
        (input, Some(comment_str))
    } else {
        (input, None)
    };

    let (input, header_crc16) = if flags & 0x02 != 0 {
        let (input, crc) = le_u16(input)?;
        (input, Some(crc))
    } else {
        (input, None)
    };

    Ok((input, GzipHeader {
        identification: identification.try_into().unwrap(),
        compression_method,
        flags,
        modification_time,
        extra_flags,
        operating_system,
        extra_fields,
        original_filename,
        file_comment,
        header_crc16,
    }))
}

fn parse_gzip_trailer(input: &[u8]) -> IResult<&[u8], GzipTrailer> {
    let (input, (crc32, original_length)) = tuple((le_u32, le_u32))(input)?;
    Ok((input, GzipTrailer { crc32, original_length }))
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    
    let (input, compressed_data) = take(input.len() - 8)(input)?;
    
    let (input, trailer) = parse_gzip_trailer(input)?;

    Ok((input, GzipFile {
        header,
        compressed_data: compressed_data.to_vec(),
        trailer,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <gzip_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_file(&buffer) {
        Ok((_, gzip_file)) => {
            println!("Parsed GZIP file: {:?}", gzip_file);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse GZIP file: {:?}", e);
            std::process::exit(1)
        }
    }
}