use nom::{
    bytes::complete::take,
    number::complete::{le_u16, le_u32, le_u8},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    cm: u8,
    flg: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
}

#[derive(Debug)]
struct GzipExtraField {
    xlen: u16,
    extra_data: Vec<u8>,
}

#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    extra_field: Option<GzipExtraField>,
    filename: Option<String>,
    comment: Option<String>,
    header_crc16: Option<u16>,
    compressed_data: Vec<u8>, // Placeholder for compressed data block interpretation
    crc32: u32,
    isize: u32,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, id1) = le_u8(input)?;
    let (input, id2) = le_u8(input)?;
    let (input, cm) = le_u8(input)?;
    let (input, flg) = le_u8(input)?;
    let (input, mtime) = le_u32(input)?;
    let (input, xfl) = le_u8(input)?;
    let (input, os) = le_u8(input)?;

    Ok((input, GzipHeader {
        id1, id2, cm, flg, mtime, xfl, os,
    }))
}

fn parse_optional_extra(input: &[u8], flag: u8) -> IResult<&[u8], Option<GzipExtraField>> {
    if flag & 0x04 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra_data) = take(xlen)(input)?;
        Ok((input, Some(GzipExtraField {
            xlen,
            extra_data: extra_data.to_vec(),
        })))
    } else {
        Ok((input, None))
    }
}

fn parse_optional_string(input: &[u8], flag: u8) -> IResult<&[u8], Option<String>> {
    if flag & 1 != 0 {
        let pos = input.iter().position(|&r| r == 0x00).unwrap() + 1;
        let (input, data) = take(pos)(input)?;
        let string = String::from_utf8_lossy(&data[..data.len() - 1]).to_string();
        Ok((input, Some(string)))
    } else {
        Ok((input, None))
    }
}

fn parse_optional_crc16(input: &[u8], flag: u8) -> IResult<&[u8], Option<u16>> {
    if flag & 0x02 != 0 {
        let (input, crc) = le_u16(input)?;
        Ok((input, Some(crc)))
    } else {
        Ok((input, None))
    }
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    let (input, extra_field) = parse_optional_extra(input, header.flg)?;
    let (input, filename) = parse_optional_string(input, header.flg & 0x08)?;
    let (input, comment) = parse_optional_string(input, header.flg & 0x10)?;
    let (input, header_crc16) = parse_optional_crc16(input, header.flg)?;
    let (input, compressed_data) = take(input.len() - 8)(input)?;
    let (tail, crc32) = le_u32(input)?;
    let (tail, isize) = le_u32(tail)?;

    Ok((tail, GzipFile {
        header,
        extra_field,
        filename,
        comment,
        header_crc16,
        compressed_data: compressed_data.to_vec(),
        crc32,
        isize,
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} <file>", args[0]);
        return Ok(())
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_file(&buffer) {
        Ok((_, gzip_file)) => println!("{:#?}", gzip_file),
        Err(e) => println!("Error parsing file: {:?}", e),
    }

    Ok(())
}