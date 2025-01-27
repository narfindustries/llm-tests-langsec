use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt},
    error::ErrorKind,
    multi::count,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::{preceded, tuple},
    IResult,
};

#[derive(Debug)]
struct GzipHeader {
    id: [u8; 2],
    compression_method: u8,
    flags: u8,
    mtime: u32,
    xflags: u8,
    os: u8,
    extra: Option<Vec<u8>>,
    filename: Option<String>,
    comment: Option<String>,
    header_crc: Option<u16>,
}

#[derive(Debug)]
struct GzipFooter {
    crc32: u32,
    isize: u32,
}

#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    compressed_data: Vec<u8>,
    footer: GzipFooter,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id, compression_method, flags, mtime, xflags, os)) = tuple((
        tag([0x1F, 0x8B]), // ID1 and ID2
        le_u8,           // Compression Method
        le_u8,           // Flags
        le_u32,          // MTIME
        le_u8,           // XFL
        le_u8,           // OS
    ))(input)?;

    let (input, extra) = if flags & 0b0000_0100 != 0 {
        let (input, extra_len) = le_u16(input)?;
        let (input, extra) = take(extra_len)(input)?;
        (input, Some(extra.to_vec()))
    } else {
        (input, None)
    };

    let (input, filename) = if flags & 0b0000_1000 != 0 {
        let (input, filename) = map_opt(take_until_null, |bytes: &[u8]| {
            std::str::from_utf8(bytes).ok().map(String::from)
        })(input)?;
        (input, Some(filename))
    } else {
        (input, None)
    };

    let (input, comment) = if flags & 0b0001_0000 != 0 {
        let (input, comment) = map_opt(take_until_null, |bytes: &[u8]| {
            std::str::from_utf8(bytes).ok().map(String::from)
        })(input)?;
        (input, Some(comment))
    } else {
        (input, None)
    };

    let (input, header_crc) = if flags & 0b0000_0010 != 0 {
        let (input, header_crc) = le_u16(input)?;
        (input, Some(header_crc))
    } else {
        (input, None)
    };

    Ok((
        input,
        GzipHeader {
            id: [0x1F, 0x8B],
            compression_method,
            flags,
            mtime,
            xflags,
            os,
            extra,
            filename,
            comment,
            header_crc,
        },
    ))
}

fn take_until_null(input: &[u8]) -> IResult<&[u8], &[u8]> {
    for i in 0..input.len() {
        if input[i] == 0 {
            return Ok((&input[i + 1..], &input[..i]));
        }
    }
    Err(nom::Err::Error((input, ErrorKind::TakeUntil)))
}

fn parse_gzip_footer(input: &[u8]) -> IResult<&[u8], GzipFooter> {
    let (input, (crc32, isize)) = tuple((le_u32, le_u32))(input)?;
    Ok((input, GzipFooter { crc32, isize }))
}

fn parse_gzip(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    let (input, compressed_data) = take(input.len() - 8)(input)?;
    let (input, footer) = parse_gzip_footer(input)?;

    Ok((
        input,
        GzipFile {
            header,
            compressed_data: compressed_data.to_vec(),
            footer,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file.gz>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)
        .expect("Failed to read file");

    match parse_gzip(&buffer) {
        Ok((_, gzip_file)) => println!("{:#?}", gzip_file),
        Err(e) => eprintln!("Failed to parse GZIP file: {:?}", e),
    }
}