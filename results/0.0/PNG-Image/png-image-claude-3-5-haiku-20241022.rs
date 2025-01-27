use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::count,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct PngChunk {
    length: u32,
    chunk_type: String,
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
    signature: [u8; 8],
    ihdr_chunk: PngChunk,
    header: PngHeader,
    chunks: Vec<PngChunk>,
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    let signature = [137, 80, 78, 71, 13, 10, 26, 10];
    map(tag(&signature), |_| signature)(input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = map(take(4usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned())(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;

    Ok((input, PngChunk {
        length,
        chunk_type,
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
    let (input, header) = preceded(tag(b"IHDR"), parse_ihdr_chunk)(&ihdr_chunk.data)?;
    
    let (input, chunks) = many0(parse_chunk)(input)?;

    Ok((input, PngImage {
        signature,
        ihdr_chunk,
        header,
        chunks,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_png(&buffer) {
        Ok((_, png)) => {
            println!("PNG Image parsed successfully:");
            println!("{:#?}", png);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing PNG: {:?}", e);
            std::process::exit(1);
        }
    }
}