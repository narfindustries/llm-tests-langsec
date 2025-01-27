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
struct PngHeader {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

#[derive(Debug)]
struct PngImage {
    signature: Vec<u8>,
    ihdr_chunk: PngChunk,
    header: PngHeader,
    chunks: Vec<PngChunk>,
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(tag(&[137, 80, 78, 71, 13, 10, 26, 10]), |v: &[u8]| v.to_vec())(input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;

    Ok((input, PngChunk {
        length,
        chunk_type: chunk_type.to_vec(),
        data: data.to_vec(),
        crc,
    }))
}

fn parse_ihdr_chunk(input: &[u8]) -> IResult<&[u8], PngHeader> {
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) = 
        tuple((
            be_u32,
            be_u32,
            be_u8,
            be_u8,
            be_u8,
            be_u8,
            be_u8
        ))(input)?;

    Ok((input, PngHeader {
        width,
        height,
        bit_depth,
        color_type,
        compression_method,
        filter_method,
        interlace_method,
    }))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, signature) = parse_png_signature(input)?;
    let (input, ihdr_chunk) = parse_chunk(input)?;
    let (input, header) = parse_ihdr_chunk(&ihdr_chunk.data)?;
    
    let (input, chunks) = count(parse_chunk, usize::MAX)(input)?;

    Ok((input, PngImage {
        signature,
        ihdr_chunk,
        header,
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
            println!("PNG Image parsed successfully:");
            println!("Width: {}", png.header.width);
            println!("Height: {}", png.header.height);
            println!("Total chunks: {}", png.chunks.len());
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse PNG: {:?}", e);
            std::process::exit(1);
        }
    }
}