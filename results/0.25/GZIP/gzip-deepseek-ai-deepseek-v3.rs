use nom::{
    bytes::complete::{take, take_until},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs, io::Read};

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: u8,
    modification_time: u32,
    extra_flags: u8,
    operating_system: u8,
    extra_field: Option<Vec<u8>>,
    original_filename: Option<String>,
    comment: Option<String>,
    header_crc16: Option<u16>,
}

#[derive(Debug)]
struct GzipTrailer {
    crc32: u32,
    uncompressed_size: u32,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, compression_method, flags, modification_time, extra_flags, operating_system)) =
        tuple((le_u8, le_u8, le_u8, le_u8, le_u32, le_u8, le_u8))(input)?;

    let (input, extra_field) = if flags & 0x04 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra_data) = take(xlen)(input)?;
        (input, Some(extra_data.to_vec()))
    } else {
        (input, None)
    };

    let (input, original_filename) = if flags & 0x08 != 0 {
        let (input, filename) = take_until(&[0u8][..])(input)?;
        let (input, _) = take(1usize)(input)?;
        (input, Some(String::from_utf8_lossy(filename).to_string()))
    } else {
        (input, None)
    };

    let (input, comment) = if flags & 0x10 != 0 {
        let (input, comment) = take_until(&[0u8][..])(input)?;
        let (input, _) = take(1usize)(input)?;
        (input, Some(String::from_utf8_lossy(comment).to_string()))
    } else {
        (input, None)
    };

    let (input, header_crc16) = if flags & 0x02 != 0 {
        let (input, crc16) = le_u16(input)?;
        (input, Some(crc16))
    } else {
        (input, None)
    };

    Ok((
        input,
        GzipHeader {
            id1,
            id2,
            compression_method,
            flags,
            modification_time,
            extra_flags,
            operating_system,
            extra_field,
            original_filename,
            comment,
            header_crc16,
        },
    ))
}

fn parse_gzip_trailer(input: &[u8]) -> IResult<&[u8], GzipTrailer> {
    let (input, (crc32, uncompressed_size)) = tuple((le_u32, le_u32))(input)?;
    Ok((input, GzipTrailer { crc32, uncompressed_size }))
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], (GzipHeader, Vec<u8>, GzipTrailer)> {
    let (input, header) = parse_gzip_header(input)?;
    let (input, compressed_data) = take(input.len() - 8)(input)?;
    let (input, trailer) = parse_gzip_trailer(input)?;
    Ok((input, (header, compressed_data.to_vec(), trailer)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip_file>", args[0]);
        return;
    }

    let mut file = fs::File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_gzip_file(&buffer) {
        Ok((_, (header, compressed_data, trailer))) => {
            println!("Header: {:?}", header);
            println!("Compressed Data Length: {}", compressed_data.len());
            println!("Trailer: {:?}", trailer);
        }
        Err(e) => eprintln!("Failed to parse GZIP file: {:?}", e),
    }
}