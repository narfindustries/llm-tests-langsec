use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, verify},
    error::ErrorKind,
    multi::many1,
    number::complete::be_u32,
    sequence::tuple,
    IResult,
};
use std::{fs::File, io::Read, path::Path, process};

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: [u8; 4],
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct Png {
    signature: [u8; 8],
    chunks: Vec<Chunk>,
}

fn parse_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    map(tag(b"\x89PNG\r\n\x1a\n"), |bytes: &[u8]| {
        bytes.try_into().expect("Expected the PNG signature to be 8 bytes")
    })(input)
}

fn parse_chunk_type(input: &[u8]) -> IResult<&[u8], [u8; 4]> {
    map(take(4usize), |bytes: &[u8]| {
        bytes.try_into().expect("Chunk type should be 4 bytes")
    })(input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = parse_chunk_type(input)?;
    let (input, data) = take(length as usize)(input)?;
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

fn parse_chunks(input: &[u8]) -> IResult<&[u8], Vec<Chunk>> {
    many1(parse_chunk)(input)
}

fn parse_png(input: &[u8]) -> IResult<&[u8], Png> {
    let (input, signature) = parse_signature(input)?;
    let (input, chunks) = parse_chunks(input)?;
    Ok((input, Png { signature, chunks }))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <path_to_png>", args[0]);
        process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path).expect("Could not open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");

    match parse_png(&buffer) {
        Ok((_, png)) => println!("{:?}", png),
        Err(e) => eprintln!("Failed to parse PNG: {:?}", e),
    }
}