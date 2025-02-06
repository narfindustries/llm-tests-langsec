use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::many1,
    number::complete::{be_u8, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum ColorType {
    Grayscale,
    RGB,
    Palette,
    GrayscaleAlpha,
    RGBAlpha,
}

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
    color_type: ColorType,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

#[derive(Debug)]
struct PngImage {
    signature: Vec<u8>,
    header: PngHeader,
    chunks: Vec<PngChunk>,
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(tag(&[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]), |v: &[u8]| v.to_vec())(input)
}

fn parse_color_type(color_type: u8) -> ColorType {
    match color_type {
        0 => ColorType::Grayscale,
        2 => ColorType::RGB,
        3 => ColorType::Palette,
        4 => ColorType::GrayscaleAlpha,
        6 => ColorType::RGBAlpha,
        _ => panic!("Invalid color type"),
    }
}

fn parse_png_header(input: &[u8]) -> IResult<&[u8], PngHeader> {
    map(
        tuple((
            be_u32,
            be_u32,
            be_u8,
            be_u8,
            be_u8,
            be_u8,
            be_u8,
        )),
        |(width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)| {
            PngHeader {
                width,
                height,
                bit_depth,
                color_type: parse_color_type(color_type),
                compression_method,
                filter_method,
                interlace_method,
            }
        },
    )(input)
}

fn parse_png_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    map(
        tuple((
            be_u32,
            map(take(4usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned()),
            map(take_data, |data| data.to_vec()),
            be_u32,
        )),
        |(length, chunk_type, data, crc)| PngChunk {
            length,
            chunk_type,
            data,
            crc,
        },
    )(input)
}

fn take_data(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let length = input[0..4].iter().fold(0, |acc, &x| (acc << 8) + x as u32) as usize;
    take(length)(input)
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    map(
        tuple((
            parse_png_signature,
            preceded(tag(b"IHDR"), parse_png_header),
            many1(parse_png_chunk),
        )),
        |(signature, header, chunks)| PngImage {
            signature,
            header,
            chunks,
        },
    )(input)
}

fn main() -> std::io::Result<()> {
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
            std::process::exit(1)
        }
    }
}