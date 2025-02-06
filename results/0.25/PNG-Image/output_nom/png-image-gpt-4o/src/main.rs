use std::fs::File;
use std::io::Read;
use std::path::Path;
use nom::{
    IResult,
    number::complete::be_u32,
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::many0,
    sequence::tuple,
};
use std::env;

#[derive(Debug)]
struct PngChunk {
    length: u32,
    chunk_type: String,
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct PngImage {
    signature: Vec<u8>,
    chunks: Vec<PngChunk>,
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, signature) = tag(&[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A])(input)?;
    Ok((input, signature.to_vec()))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = map_res(take(4usize), |bytes: &[u8]| std::str::from_utf8(bytes).map(|s| s.to_string()))(input)?;
    let (input, data) = take(length as usize)(input)?;
    let (input, crc) = be_u32(input)?;

    Ok((input, PngChunk {
        length,
        chunk_type,
        data: data.to_vec(),
        crc,
    }))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, signature) = parse_png_signature(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;

    Ok((input, PngImage { signature, chunks }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
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
    if let Err(err) = file.read_to_end(&mut buffer) {
        eprintln!("Error reading file: {}", err);
        std::process::exit(1);
    }

    match parse_png(&buffer) {
        Ok((_, png_image)) => {
            println!("{:?}", png_image);
        }
        Err(err) => {
            eprintln!("Error parsing PNG: {:?}", err);
            std::process::exit(1);
        }
    }
}