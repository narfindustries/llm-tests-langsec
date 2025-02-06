use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
    extra_field: Option<Vec<u8>>,
    filename: Option<String>,
    comment: Option<String>,
    header_crc16: Option<u16>,
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

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, id1) = u8(input)?;
    let (input, id2) = u8(input)?;
    let (input, compression_method) = u8(input)?;
    let (input, flags) = u8(input)?;
    let (input, mtime) = le_u32(input)?;
    let (input, xfl) = u8(input)?;
    let (input, os) = u8(input)?;

    let mut input = input;
    let mut extra_field = None;
    let mut filename = None;
    let mut comment = None;
    let mut header_crc16 = None;

    // Parse optional fields based on flags
    if flags & 0x04 != 0 {
        // FEXTRA
        let (remaining, xlen) = le_u16(input)?;
        let (remaining, extra) = take(xlen)(remaining)?;
        extra_field = Some(extra.to_vec());
        input = remaining;
    }

    if flags & 0x08 != 0 {
        // FNAME
        let (remaining, fname) = parse_null_terminated_string(input)?;
        filename = Some(fname);
        input = remaining;
    }

    if flags & 0x10 != 0 {
        // FCOMMENT
        let (remaining, fcomment) = parse_null_terminated_string(input)?;
        comment = Some(fcomment);
        input = remaining;
    }

    if flags & 0x02 != 0 {
        // FHCRC
        let (remaining, crc16) = le_u16(input)?;
        header_crc16 = Some(crc16);
        input = remaining;
    }

    Ok((
        input,
        GzipHeader {
            id1,
            id2,
            compression_method,
            flags,
            mtime,
            xfl,
            os,
            extra_field,
            filename,
            comment,
            header_crc16,
        },
    ))
}

fn parse_gzip_file(input: &[u8]) -> IResult<&[u8], GzipFile> {
    let (input, header) = parse_gzip_header(input)?;
    
    // The compressed data extends until the last 8 bytes (CRC32 + ISIZE)
    let compressed_data_len = input.len() - 8;
    let (input, compressed_data) = take(compressed_data_len)(input)?;
    let (input, crc32) = le_u32(input)?;
    let (input, isize) = le_u32(input)?;

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

fn main() -> std::io::Result<()> {
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
            println!("Header:");
            println!("  ID1: 0x{:02X}", gzip_file.header.id1);
            println!("  ID2: 0x{:02X}", gzip_file.header.id2);
            println!("  Compression Method: {}", gzip_file.header.compression_method);
            println!("  Flags: 0x{:02X}", gzip_file.header.flags);
            println!("  MTIME: {}", gzip_file.header.mtime);
            println!("  XFL: {}", gzip_file.header.xfl);
            println!("  OS: {}", gzip_file.header.os);
            
            if let Some(extra) = &gzip_file.header.extra_field {
                println!("  Extra Field: {:?}", extra);
            }
            if let Some(filename) = &gzip_file.header.filename {
                println!("  Filename: {}", filename);
            }
            if let Some(comment) = &gzip_file.header.comment {
                println!("  Comment: {}", comment);
            }
            if let Some(crc16) = gzip_file.header.header_crc16 {
                println!("  Header CRC16: 0x{:04X}", crc16);
            }
            
            println!("Compressed Data Length: {}", gzip_file.compressed_data.len());
            println!("CRC32: 0x{:08X}", gzip_file.crc32);
            println!("ISIZE: {}", gzip_file.isize);
        }
        Err(e) => {
            eprintln!("Error parsing GZIP file: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}