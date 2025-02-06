extern crate nom;
use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
enum ChunkType {
    IHDR,
    PLTE,
    IDAT,
    IEND,
    Unknown(String),
}

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: ChunkType,
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct PngImage {
    signature: [u8; 8],
    chunks: Vec<Chunk>,
}

fn parse_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    let (input, signature) = tag([137, 80, 78, 71, 13, 10, 26, 10])(input)?;
    Ok((input, signature.try_into().unwrap()))
}

fn parse_chunk_type(input: &[u8]) -> IResult<&[u8], ChunkType> {
    let (input, chunk_type_bytes) = take(4usize)(input)?;
    let chunk_type_str = std::str::from_utf8(chunk_type_bytes).unwrap();
    let chunk_type = match chunk_type_str {
        "IHDR" => ChunkType::IHDR,
        "PLTE" => ChunkType::PLTE,
        "IDAT" => ChunkType::IDAT,
        "IEND" => ChunkType::IEND,
        _ => ChunkType::Unknown(chunk_type_str.to_string()),
    };
    Ok((input, chunk_type))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = parse_chunk_type(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;
    Ok((
        input,
        Chunk {
            length,
            chunk_type,
            data: data.to_vec(),
            crc,
        },
    ))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, signature) = parse_signature(input)?;
    let mut chunks = Vec::new();
    let mut remaining_input = input;

    while !remaining_input.is_empty() {
        let (new_input, chunk) = parse_chunk(remaining_input)?;
        remaining_input = new_input;
        chunks.push(chunk);
        if let Some(Chunk {
            chunk_type: ChunkType::IEND,
            ..
        }) = chunks.last()
        {
            break;
        }
    }

    Ok((remaining_input, PngImage { signature, chunks }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = fs::read(filename).expect("Unable to read file");

    match parse_png(&data) {
        Ok((_, png)) => println!("{:?}", png),
        Err(e) => eprintln!("Failed to parse PNG: {:?}", e),
    }
}