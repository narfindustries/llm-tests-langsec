use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{tuple, preceded}
};

#[derive(Debug)]
struct PngHeader {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8
}

#[derive(Debug)]
struct PngChunk {
    length: u32,
    chunk_type: Vec<u8>,
    data: Vec<u8>,
    crc: u32
}

#[derive(Debug)]
struct PngImage {
    header: Vec<u8>,
    chunks: Vec<PngChunk>
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], &[u8]> {
    tag(b"\x89PNG\r\n\x1a\n")(input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    map(
        tuple((
            be_u32,
            take(4usize),
            take_some_length,
            be_u32
        )),
        |(length, chunk_type, data, crc)| PngChunk {
            length,
            chunk_type: chunk_type.to_vec(),
            data: data.to_vec(),
            crc
        }
    )(input)
}

fn take_some_length(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take(input[0..4].iter().fold(0, |acc, x| acc * 256 + *x as usize))(input)
}

fn parse_ihdr_chunk(input: &[u8]) -> IResult<&[u8], PngHeader> {
    map(
        tuple((
            be_u32, // width
            be_u32, // height
            be_u8,  // bit depth
            be_u8,  // color type
            be_u8,  // compression method
            be_u8,  // filter method
            be_u8   // interlace method
        )),
        |(width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)| 
            PngHeader {
                width,
                height,
                bit_depth,
                color_type,
                compression_method,
                filter_method,
                interlace_method
            }
    )(input)
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    map(
        tuple((
            parse_png_signature,
            parse_chunk,
            many0(parse_chunk)
        )),
        |(_, header_chunk, chunks)| PngImage {
            header: header_chunk.data.clone(),
            chunks
        }
    )(input)
}

fn main() -> std::io::Result<()> {
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
            println!("Successfully parsed PNG: {:?}", png);
            Ok(())
        },
        Err(e) => {
            eprintln!("Error parsing PNG: {:?}", e);
            std::process::exit(1)
        }
    }
}