use nom::bytes::complete::{tag, take};
use nom::combinator::{cond, map};
use nom::number::complete::{le_u16, le_u32, le_u8};
use nom::sequence::tuple;
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    cm: u8,
    flg: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
    extra: Option<Vec<u8>>,
    name: Option<String>,
    comment: Option<String>,
    crc16: Option<u16>,
}

#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    compressed_data: Vec<u8>,
    crc32: u32,
    isize: u32,
}

fn parse_null_terminated_string(input: &[u8]) -> IResult<&[u8], String> {
    let mut i = 0;
    while i < input.len() && input[i] != 0 {
        i += 1;
    }
    let (remaining, string_bytes) = take(i)(input)?;
    let (remaining, _) = tag(&[0])(remaining)?;
    Ok((
        remaining,
        String::from_utf8_lossy(string_bytes).to_string(),
    ))
}

fn parse_extra_field(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, xlen) = le_u16(input)?;
    let (input, data) = take(xlen as usize)(input)?;
    Ok((input, data.to_vec()))
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, cm, flg)) = tuple((le_u8, le_u8, le_u8, le_u8))(input)?;
    let (input, mtime) = le_u32(input)?;
    let (input, (xfl, os)) = tuple((le_u8, le_u8))(input)?;

    let fextra = (flg & 0x04) != 0;
    let fname = (flg & 0x08) != 0;
    let fcomment = (flg & 0x10) != 0;
    let fhcrc = (flg & 0x02) != 0;

    let (input, extra) = cond(fextra, parse_extra_field)(input)?;
    let (input, name) = cond(fname, parse_null_terminated_string)(input)?;
    let (input, comment) = cond(fcomment, parse_null_terminated_string)(input)?;
    let (input, crc16) = cond(fhcrc, le_u16)(input)?;

    Ok((
        input,
        GzipHeader {
            id1,
            id2,
            cm,
            flg,
            mtime,
            xfl,
            os,
            extra,
            name,
            comment,
            crc16,
        },
    ))
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    
    let compressed_data_len = input.len() - 8;
    let (input, compressed_data) = take(compressed_data_len)(input)?;
    let (input, (crc32, isize)) = tuple((le_u32, le_u32))(input)?;

    Ok((
        input,
        GzipFile {
            header,
            compressed_data: compressed_data.to_vec(),
            crc32,
            isize,
        },
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip-file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_file(&buffer) {
        Ok((_, gzip_file)) => {
            println!("GZIP Header: {:#?}", gzip_file.header);
            println!("Compressed data length: {} bytes", gzip_file.compressed_data.len());
            println!("CRC32: {:08x}", gzip_file.crc32);
            println!("Original size: {} bytes", gzip_file.isize);
        }
        Err(e) => eprintln!("Failed to parse GZIP file: {:?}", e),
    }

    Ok(())
}