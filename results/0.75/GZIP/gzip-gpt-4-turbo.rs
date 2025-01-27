use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: u8,
    mtime: u32,
    extra_flags: u8,
    os: u8,
    extra: Option<Vec<u8>>,
    filename: Option<String>,
    comment: Option<String>,
    crc16: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, compression_method, flags, mtime, extra_flags, os)) = tuple((
        le_u8,
        le_u8,
        le_u8,
        le_u8,
        le_u32,
        le_u8,
        le_u8,
    ))(input)?;

    let (input, extra) = if flags & 0b100 != 0 {
        let (input, xlen) = le_u16(input)?;
        let (input, extra) = take(xlen as usize)(input)?;
        (input, Some(extra.to_vec()))
    } else {
        (input, None)
    };

    let (mut input, filename) = if flags & 0b1000 != 0 {
        let mut result = vec![];
        loop {
            let (inner_input, byte) = le_u8(input)?;
            input = inner_input;
            if byte == 0 {
                break;
            }
            result.push(byte);
        }
        (input, Some(String::from_utf8_lossy(&result).to_string()))
    } else {
        (input, None)
    };

    let (mut input, comment) = if flags & 0b10000 != 0 {
        let mut result = vec![];
        loop {
            let (inner_input, byte) = le_u8(input)?;
            input = inner_input;
            if byte == 0 {
                break;
            }
            result.push(byte);
        }
        (input, Some(String::from_utf8_lossy(&result).to_string()))
    } else {
        (input, None)
    };

    let (input, crc16) = if flags & 0b10 != 0 {
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
            mtime,
            extra_flags,
            os,
            extra,
            filename,
            comment,
            crc16,
        },
    ))
}

fn read_gzip_file(filepath: &str) -> io::Result<()> {
    let mut file = File::open(filepath)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_header(&buffer) {
        Ok((_remaining, header)) => {
            println!("{:#?}", header);
        }
        Err(e) => {
            println!("Failed to parse GZIP header: {:?}", e);
        }
    }

    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} <FILE>", args[0]);
        return;
    }
    let filepath = &args[1];

    if let Err(e) = read_gzip_file(filepath) {
        eprintln!("Error reading file: {}", e);
    }
}