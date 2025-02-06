use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::many0,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct GzipHeader {
    identification: (u8, u8),
    compression_method: u8,
    flags: u8,
    modification_time: u32,
    extra_flags: u8,
    operating_system: u8,
    extra_fields: Option<Vec<u8>>,
    original_filename: Option<String>,
    file_comment: Option<String>,
    header_crc: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2)) = tuple((le_u8, le_u8))(input)?;
    let (input, compression_method) = le_u8(input)?;
    let (input, flags) = le_u8(input)?;
    let (input, modification_time) = le_u32(input)?;
    let (input, extra_flags) = le_u8(input)?;
    let (input, operating_system) = le_u8(input)?;

    let (input, extra_fields) = if flags & 0x04 != 0 {
        let (input, extra_len) = le_u16(input)?;
        let (input, extra_data) = take(extra_len as usize)(input)?;
        (input, Some(extra_data.to_vec()))
    } else {
        (input, None)
    };

    let (input, original_filename) = if flags & 0x08 != 0 {
        let (input, filename) = many0(map(take(1usize), |b: &[u8]| b[0] as char))(input)?;
        let filename: String = filename.into_iter()
            .take_while(|&c| c != '\0')
            .collect();
        (input, Some(filename))
    } else {
        (input, None)
    };

    let (input, file_comment) = if flags & 0x10 != 0 {
        let (input, comment) = many0(map(take(1usize), |b: &[u8]| b[0] as char))(input)?;
        let comment: String = comment.into_iter()
            .take_while(|&c| c != '\0')
            .collect();
        (input, Some(comment))
    } else {
        (input, None)
    };

    let (input, header_crc) = if flags & 0x02 != 0 {
        let (input, crc) = le_u16(input)?;
        (input, Some(crc))
    } else {
        (input, None)
    };

    Ok((input, GzipHeader {
        identification: (id1, id2),
        compression_method,
        flags,
        modification_time,
        extra_flags,
        operating_system,
        extra_fields,
        original_filename,
        file_comment,
        header_crc,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gzip_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => {
            println!("Parsed GZIP Header: {:?}", header);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse GZIP header: {:?}", e);
            std::process::exit(1);
        }
    }
}