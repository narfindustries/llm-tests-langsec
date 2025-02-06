use nom::{
    bytes::complete::take,
    combinator::{cond, map_opt},
    multi::many0,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

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
    file_comment: Option<String>,
    header_crc16: Option<u16>,
}

fn parse_gzip_header(input: &[u8]) -> IResult<&[u8], GzipHeader> {
    let (input, (id1, id2, compression_method, flags, modification_time, extra_flags, operating_system)) = 
        tuple((le_u8, le_u8, le_u8, le_u8, le_u32, le_u8, le_u8))(input)?;

    let (input, extra_field) = cond(flags & 0x04 != 0, |i: &[u8]| -> IResult<&[u8], Vec<u8>> {
        let (i, extra_len) = le_u16(i)?;
        let (i, extra_data) = take(extra_len as usize)(i)?;
        Ok((i, extra_data.to_vec()))
    })(input)?;

    let (input, original_filename) = cond(flags & 0x08 != 0, |i: &[u8]| -> IResult<&[u8], String> {
        let (i, name) = many0(map_opt(le_u8, |b| if b == 0 { None } else { Some(b as char) }))(i)?;
        Ok((i, name.into_iter().collect()))
    })(input)?;

    let (input, file_comment) = cond(flags & 0x10 != 0, |i: &[u8]| -> IResult<&[u8], String> {
        let (i, comment) = many0(map_opt(le_u8, |b| if b == 0 { None } else { Some(b as char) }))(i)?;
        Ok((i, comment.into_iter().collect()))
    })(input)?;

    let (input, header_crc16) = cond(flags & 0x02 != 0, le_u16)(input)?;

    Ok((input, GzipHeader {
        id1,
        id2,
        compression_method,
        flags,
        modification_time,
        extra_flags,
        operating_system,
        extra_field,
        original_filename,
        file_comment,
        header_crc16,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <gzip_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gzip_header(&buffer) {
        Ok((_, header)) => {
            println!("Parsed GZIP Header: {:?}", header);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing GZIP header: {:?}", e);
            Err("Parsing failed".into())
        }
    }
}