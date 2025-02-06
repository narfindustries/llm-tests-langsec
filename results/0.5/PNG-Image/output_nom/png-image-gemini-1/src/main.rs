use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, rest},
    error::Error,
    number::complete::{be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
enum ChunkType {
    IHDR,
    IDAT,
    IEND,
    PLTE,
    tRNS,
    gAMA,
    // Add other chunk types here as needed...
    Other(Vec<u8>),
}

impl From<&[u8; 4]> for ChunkType {
    fn from(bytes: &[u8; 4]) -> Self {
        match bytes {
            b"IHDR" => ChunkType::IHDR,
            b"IDAT" => ChunkType::IDAT,
            b"IEND" => ChunkType::IEND,
            b"PLTE" => ChunkType::PLTE,
            b"tRNS" => ChunkType::tRNS,
            b"gAMA" => ChunkType::gAMA,
            _ => ChunkType::Other(bytes.to_vec()),
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


fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk, Error<&[u8]>> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = map(take(4usize), |bytes: &[u8]| {
        let mut arr = [0u8; 4];
        arr.copy_from_slice(bytes);
        ChunkType::from(&arr)
    })(input)?;
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

fn parse_png(input: &[u8]) -> IResult<&[u8], Vec<Chunk>, Error<&[u8]>> {
    let (input, _) = tag(b"\x89PNG\r\n\x1a\n")(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;
    Ok((input, chunks))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            std::process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            std::process::exit(1);
        }
    };

    match parse_png(&buffer) {
        Ok((_, chunks)) => {
            println!("PNG Chunks: {:?}", chunks);
        }
        Err(e) => {
            println!("Error parsing PNG: {:?}", e);
        }
    }
}

use nom::multi::many0;
