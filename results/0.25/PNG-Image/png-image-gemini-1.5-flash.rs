use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, rest},
    error::{context, ErrorKind, ParseError},
    multi::count,
    number::complete::{be_u32, le_u32},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct PngSignature {
    signature: [u8; 8],
}

fn png_signature(input: &[u8]) -> IResult<&[u8], PngSignature> {
    map(tag(b"\x89PNG\r\n\x1a\n"), |_| PngSignature {
        signature: [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a],
    })(input)
}

#[derive(Debug)]
struct PngChunk {
    length: u32,
    chunk_type: [u8; 4],
    chunk_data: Vec<u8>,
    crc: u32,
}

fn png_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let (input, chunk_data) = take(length as usize)(input)?;
    let (input, crc) = be_u32(input)?;
    Ok((
        input,
        PngChunk {
            length,
            chunk_type: chunk_type.try_into().unwrap(),
            chunk_data: chunk_data.to_vec(),
            crc,
        },
    ))
}

#[derive(Debug)]
struct PngImage {
    signature: PngSignature,
    chunks: Vec<PngChunk>,
}

fn png_image(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, signature) = png_signature(input)?;
    let (input, chunks) = many0(png_chunk)(input)?;
    let (input, _) = tag(b"IEND")(input)?; //check for IEND
    Ok((
        input,
        PngImage {
            signature,
            chunks,
        },
    ))
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
            eprintln!("Failed to open file: {}", err);
            std::process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Failed to read file: {}", err);
            std::process::exit(1);
        }
    };

    match png_image(&buffer) {
        Ok((_, image)) => println!("PNG Image: {:?}", image),
        Err(err) => {
            eprintln!("Failed to parse PNG: {:?}", err);
            std::process::exit(1);
        }
    }
}

use nom::multi::many0;
