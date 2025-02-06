use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::length_data,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct PNGFile {
    signature: [u8; 8],
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

#[derive(Debug)]
struct IDAT {
    data: Vec<u8>,
}

#[derive(Debug)]
struct IEND;

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    map(tag(&[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]), |sig| {
        let mut arr = [0u8; 8];
        arr.copy_from_slice(sig);
        arr
    })(input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, (length, chunk_type, data, crc)) = tuple((
        be_u32,
        map(take(4usize), |s: &[u8]| String::from_utf8_lossy(s).into_owned()),
        length_data(be_u32),
        be_u32,
    ))(input)?;
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

fn parse_ihdr(input: &[u8]) -> IResult<&[u8], IHDR> {
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) = tuple((
        be_u32, be_u32, be_u8, be_u8, be_u8, be_u8, be_u8,
    ))(input)?;
    Ok((
        input,
        IHDR {
            width,
            height,
            bit_depth,
            color_type,
            compression_method,
            filter_method,
            interlace_method,
        },
    ))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PNGFile> {
    let (input, signature) = parse_png_signature(input)?;
    let mut chunks = Vec::new();
    let mut input = input;
    while !input.is_empty() {
        let (remaining, chunk) = parse_chunk(input)?;
        chunks.push(chunk);
        input = remaining;
    }
    Ok((
        input,
        PNGFile {
            signature,
            chunks,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_png(&buffer) {
        Ok((_, png)) => {
            println!("{:#?}", png);
        }
        Err(e) => eprintln!("Failed to parse PNG: {:?}", e),
    }
}