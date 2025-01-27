use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, verify},
    multi::many0,
    number::complete::{be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;
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
    map(tag(&[0x89, b'P', b'N', b'G', b'\r', b'\n', 0x1A, b'\n']), |sig: &[u8]| {
        sig.to_vec()
    })(input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = map_res(take(4usize), |bytes: &[u8]| {
        std::str::from_utf8(bytes).map(|s| s.to_string())
    })(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;

    Ok((
        input,
        PngChunk {
            length,
            chunk_type,
            data: data.to_vec(),
            crc,
        },
    ))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, signature) = parse_png_signature(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;

    Ok((
        input,
        PngImage {
            signature,
            chunks,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path).expect("Could not open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");

    match parse_png(&buffer) {
        Ok((_, png)) => {
            println!("{:#?}", png);
        }
        Err(e) => {
            eprintln!("Failed to parse PNG file: {:?}", e);
        }
    }
}