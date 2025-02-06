use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: u8,
    modification_time: u32,
    extra_flags: u8,
    os: u8,
    extra_field: Option<Vec<u8>>,
    original_filename: Option<String>,
    comment: Option<String>,
    header_crc16: Option<u16>,
}

#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    compressed_data: Vec<u8>,
    crc32: u32,
    uncompressed_size: u32,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, compression_method, flags, modification_time, extra_flags, os)) =
        tuple((le_u8, le_u8, le_u8, le_u8, le_u32, le_u8, le_u8))(input)?;

    let (input, extra_field) = if flags & 0x04 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra) = take(xlen)(input)?;
        (input, Some(extra.to_vec()))
    } else {
        (input, None)
    };

    let (input, original_filename) = if flags & 0x08 != 0 {
        let (input, filename) = take_until_null(input)?;
        (input, Some(String::from_utf8_lossy(filename).into_owned()))
    } else {
        (input, None)
    };

    let (input, comment) = if flags & 0x10 != 0 {
        let (input, comment) = take_until_null(input)?;
        (input, Some(String::from_utf8_lossy(comment).into_owned()))
    } else {
        (input, None)
    };

    let (input, header_crc16) = if flags & 0x02 != 0 {
        let (input, crc) = le_u16(input)?;
        (input, Some(crc))
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
            os,
            extra_field,
            original_filename,
            comment,
            header_crc16,
        },
    ))
}

fn take_until_null(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let null_pos = input.iter().position(|&x| x == 0).unwrap_or(input.len());
    let (input, data) = take(null_pos)(input)?;
    let (input, _) = take(1usize)(input)?; // Consume the null byte
    Ok((input, data))
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    let (input, compressed_data) = take(input.len() - 8)(input)?;
    let (input, (crc32, uncompressed_size)) = tuple((le_u32, le_u32))(input)?;

    Ok((
        input,
        GzipFile {
            header,
            compressed_data: compressed_data.to_vec(),
            crc32,
            uncompressed_size,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_file(&buffer) {
        Ok((_, gzip_file)) => println!("{:#?}", gzip_file),
        Err(e) => eprintln!("Failed to parse GZIP file: {:?}", e),
    }

    Ok(())
}