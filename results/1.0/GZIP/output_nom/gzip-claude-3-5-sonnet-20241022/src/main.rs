use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    error::ErrorKind,
    multi::many0,
    number::complete::{le_u16, le_u32, u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

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
    name: Option<Vec<u8>>,
    comment: Option<Vec<u8>>,
    crc16: Option<u16>,
}

#[derive(Debug)]
struct GzipFile {
    header: GzipHeader,
    compressed_data: Vec<u8>,
    crc32: u32,
    isize: u32,
}

fn parse_extra_field(input: &[u8], flg: u8) -> IResult<&[u8], Option<Vec<u8>>> {
    if (flg & 0x04) != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra) = take(xlen)(input)?;
        Ok((input, Some(extra.to_vec())))
    } else {
        Ok((input, None))
    }
}

fn parse_zero_terminated_field(input: &[u8], flag_bit: u8, flg: u8) -> IResult<&[u8], Option<Vec<u8>>> {
    if (flg & flag_bit) != 0 {
        let mut result = Vec::new();
        let mut current_input = input;
        
        loop {
            let (remaining, byte) = u8(current_input)?;
            if byte == 0 {
                return Ok((remaining, Some(result)));
            }
            result.push(byte);
            current_input = remaining;
        }
    } else {
        Ok((input, None))
    }
}

fn parse_crc16(input: &[u8], flg: u8) -> IResult<&[u8], Option<u16>> {
    if (flg & 0x02) != 0 {
        let (input, crc) = le_u16(input)?;
        Ok((input, Some(crc)))
    } else {
        Ok((input, None))
    }
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2)) = tuple((
        verify(u8, |&x| x == 0x1f),
        verify(u8, |&x| x == 0x8b),
    ))(input)?;
    
    let (input, cm) = verify(u8, |&x| x == 8)(input)?;
    let (input, flg) = u8(input)?;
    let (input, mtime) = le_u32(input)?;
    let (input, xfl) = u8(input)?;
    let (input, os) = u8(input)?;
    
    let (input, extra) = parse_extra_field(input, flg)?;
    let (input, name) = parse_zero_terminated_field(input, 0x08, flg)?;
    let (input, comment) = parse_zero_terminated_field(input, 0x10, flg)?;
    let (input, crc16) = parse_crc16(input, flg)?;
    
    Ok((input, GzipHeader {
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
    }))
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    
    // Find the last 8 bytes (CRC32 and ISIZE)
    let data_end = input.len() - 8;
    let (trailer, compressed_data) = input.split_at(data_end);
    
    let (trailer, crc32) = le_u32(trailer)?;
    let (trailer, isize) = le_u32(trailer)?;
    
    Ok((trailer, GzipFile {
        header,
        compressed_data: compressed_data.to_vec(),
        crc32,
        isize,
    }))
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
            println!("GZIP File Structure:");
            println!("Header: {:?}", gzip_file.header);
            println!("Compressed data length: {} bytes", gzip_file.compressed_data.len());
            println!("CRC32: {:08x}", gzip_file.crc32);
            println!("Input size (mod 2^32): {}", gzip_file.isize);
        }
        Err(e) => eprintln!("Error parsing GZIP file: {:?}", e),
    }

    Ok(())
}