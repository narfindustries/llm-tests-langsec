use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    error::Error,
    number::complete::{be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use crc32fast::Hasher;

#[derive(Debug)]
struct PngHeader {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

#[derive(Debug)]
enum ChunkType {
    IHDR,
    IDAT,
    IEND,
    Other(Vec<u8>),
}

fn parse_chunk_type(input: &[u8]) -> IResult<&[u8], ChunkType> {
    let (input, chunk_type) = take(4usize)(input)?;
    let chunk_type_str = std::str::from_utf8(chunk_type).unwrap();

    match chunk_type_str {
        "IHDR" => Ok((input, ChunkType::IHDR)),
        "IDAT" => Ok((input, ChunkType::IDAT)),
        "IEND" => Ok((input, ChunkType::IEND)),
        _ => Ok((input, ChunkType::Other(chunk_type.to_vec()))),
    }
}

fn parse_chunk_data(input: &[u8], length: u32) -> IResult<&[u8], Vec<u8>> {
    let (input, data) = take(length as usize)(input)?;
    Ok((input, data.to_vec()))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], (ChunkType, Vec<u8>)> {
    let (input, (length, chunk_type, data, crc)) = tuple((be_u32, parse_chunk_type, |i| parse_chunk_data(i, length), be_u32))(input)?;

    let mut hasher = Hasher::new();
    hasher.update(&length.to_be_bytes());
    hasher.update(chunk_type);
    hasher.update(&data);
    let computed_crc = hasher.finalize();

    if computed_crc != crc {
        return Err(nom::Err::Error(Error::new(input,nom::error::ErrorKind::Verify)));
    }

    Ok((input, (chunk_type, data)))
}

fn parse_png_header(input: &[u8]) -> IResult<&[u8], PngHeader> {
    let (input, _) = tag(b"\x89PNG\r\n\x1a\n")(input)?;
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) = tuple((be_u32, be_u32, be_u8, be_u8, be_u8, be_u8, be_u8))(input)?;

    Ok((input, PngHeader { width, height, bit_depth, color_type, compression_method, filter_method, interlace_method }))
}

use nom::multi::many0;

fn parse_png(input: &[u8]) -> IResult<&[u8], (PngHeader, Vec<(ChunkType, Vec<u8>)>)> {
    let (input, header) = parse_png_header(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;
    Ok((input, (header, chunks)))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <filename>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_png(&buffer) {
        Ok((_, (header, chunks))) => {
            println!("PNG Header: {:?}", header);
            println!("Chunks:");
            for (chunk_type, data) in chunks {
                println!("  Type: {:?}, Data length: {}", chunk_type, data.len());
            }
        }
        Err(e) => {
            println!("Error parsing PNG: {:?}", e);
        }
    }
}