use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::count,
    number::complete::{be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct PngChunk {
    length: u32,
    chunk_type: Vec<u8>,
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct PngImage {
    signature: Vec<u8>,
    chunks: Vec<PngChunk>,
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let signature = vec![137, 80, 78, 71, 13, 10, 26, 10];
    map(tag(&signature), |s: &[u8]| s.to_vec())(input)
}

fn parse_png_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let (input, data) = take(length as usize)(input)?;
    let (input, crc) = be_u32(input)?;

    Ok((input, PngChunk {
        length,
        chunk_type: chunk_type.to_vec(),
        data: data.to_vec(),
        crc,
    }))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, signature) = parse_png_signature(input)?;
    let (input, chunks) = many0(parse_png_chunk)(input)?;

    Ok((input, PngImage {
        signature,
        chunks,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_png(&buffer) {
        Ok((_, png)) => {
            println!("PNG Image parsed successfully: {:?}", png);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing PNG: {:?}", e);
            Err(e.into())
        }
    }
}