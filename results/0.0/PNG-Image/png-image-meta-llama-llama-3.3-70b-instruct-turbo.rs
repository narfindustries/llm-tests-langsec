use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{length_data, many_till},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
enum ChunkType {
    IHDR,
    PLTE,
    IDAT,
    IEND,
    Other(u32),
}

impl ChunkType {
    fn from_u32(n: u32) -> Self {
        match n {
            0x49484452 => ChunkType::IHDR,
            0x504c5445 => ChunkType::PLTE,
            0x49444154 => ChunkType::IDAT,
            0x49454e44 => ChunkType::IEND,
            _ => ChunkType::Other(n),
        }
    }
}

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: ChunkType,
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct Png {
    signature: [u8; 8],
    chunks: Vec<Chunk>,
}

fn parse_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    tag(&[0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a])(input)
}

fn parse_chunk_type(input: &[u8]) -> IResult<&[u8], ChunkType> {
    map_res(be_u32, ChunkType::from_u32)(input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = parse_chunk_type(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;
    Ok((input, Chunk { length, chunk_type, data: data.to_vec(), crc }))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], Png> {
    let (input, signature) = parse_signature(input)?;
    let (input, chunks) = many_till(parse_chunk, tag(&[0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44, 0x00, 0x00, 0x00, 0x00]))(input)?;
    Ok((input, Png { signature: signature.try_into().unwrap(), chunks }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (_rest, png) = parse_png(&input).unwrap();
    println!("{:?}", png);
}