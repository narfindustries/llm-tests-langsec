use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct PNGFile {
    signature: Vec<u8>,
    chunks: Vec<Chunk>,
}

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: String,
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct IHDR {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, signature) = tag([0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A])(input)?;
    Ok((input, signature.to_vec()))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4u8)(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;
    Ok((input, Chunk {
        length,
        chunk_type: String::from_utf8(chunk_type.to_vec()).unwrap(),
        data: data.to_vec(),
        crc,
    }))
}

fn parse_ihdr(input: &[u8]) -> IResult<&[u8], IHDR> {
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) = tuple((
        be_u32,
        be_u32,
        be_u8,
        be_u8,
        be_u8,
        be_u8,
        be_u8,
    ))(input)?;
    Ok((input, IHDR {
        width,
        height,
        bit_depth,
        color_type,
        compression_method,
        filter_method,
        interlace_method,
    }))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PNGFile> {
    let (input, signature) = parse_png_signature(input)?;
    let mut chunks = Vec::new();
    let mut remaining_input = input;
    while !remaining_input.is_empty() {
        let (input, chunk) = parse_chunk(remaining_input)?;
        chunks.push(chunk);
        remaining_input = input;
    }
    Ok((remaining_input, PNGFile { signature, chunks }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        return;
    }
    let file_path = &args[1];
    let data = fs::read(file_path).expect("Unable to read file");
    let (_, png_file) = parse_png(&data).expect("Failed to parse PNG file");
    println!("{:#?}", png_file);
}