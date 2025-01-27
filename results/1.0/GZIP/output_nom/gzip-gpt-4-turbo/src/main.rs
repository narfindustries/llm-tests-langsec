use nom::{
    bytes::complete::take,
    number::complete::{le_u16, le_u32, le_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug, PartialEq)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: u8,
    mtime: u32,
    extra_flags: u8,
    os: u8,
    extra: Option<Vec<u8>>,
    original_filename: Option<String>,
    comment: Option<String>,
}

#[derive(Debug, PartialEq)]
struct GzipFile {
    header: GzipHeader,
    compressed_data: Vec<u8>,
    crc32: u32,
    input_size: u32,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, id1) = le_u8(input)?;
    let (input, id2) = le_u8(input)?;
    let (input, compression_method) = le_u8(input)?;
    let (input, flags) = le_u8(input)?;
    let (input, mtime) = le_u32(input)?;
    let (input, extra_flags) = le_u8(input)?;
    let (input, os) = le_u8(input)?;

    let (input, extra) = if flags & 0b100 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra) = take(xlen as usize)(input)?;
        (input, Some(extra.to_vec()))
    } else {
        (input, None)
    };

    let (input, original_filename) = if flags & 0b1000 != 0 {
        let (input, filename) = nom::character::complete::cstr(input)?;
        (input, Some(filename.to_owned()))
    } else {
        (input, None)
    };

    let (input, comment) = if flags & 0b10000 != 0 {
        let (input, comment) = nom::character::complete::cstr(input)?;
        (input, Some(comment.to_owned()))
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
            mtime,
            extra_flags,
            os,
            extra,
            original_filename,
            comment,
        },
    ))
}

fn parse_gzip(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    let (input, compressed_data) = take(input.len() - 8)(input)?; // excluding CRC32 and Input Size
    let (input, crc32) = le_u32(input)?;
    let (input, input_size) = le_u32(input)?;

    Ok((
        input,
        GzipFile {
            header,
            compressed_data: compressed_data.to_vec(),
            crc32,
            input_size,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Usage: gzip_parser <file_path>",
        ));
    }

    let filepath = &args[1];
    let mut file = File::open(filepath)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip(&buffer) {
        Ok((_, gzip_file)) => println!("{:#?}", gzip_file),
        Err(e) => println!("Failed to parse GZIP file: {:?}", e),
    }

    Ok(())
}