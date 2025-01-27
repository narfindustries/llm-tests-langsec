use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::many0,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

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
struct Chunk {
    length: u32,
    chunk_type: String,
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct PngImage {
    header: PngHeader,
    chunks: Vec<Chunk>,
}

fn parse_png_header(input: &[u8]) -> IResult<&[u8], PngHeader> {
    let (input, _) = tag(b"\x89PNG\r\n\x1a\n")(input)?;
    let (input, _) = parse_chunk(input, b"IHDR")?;
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) = tuple((
        be_u32,
        be_u32,
        nom::number::complete::u8,
        nom::number::complete::u8,
        nom::number::complete::u8,
        nom::number::complete::u8,
        nom::number::complete::u8,
    ))(input)?;

    Ok((
        input,
        PngHeader {
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

fn parse_chunk(input: &[u8], expected_type: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;

    if &chunk_type != expected_type {
        return Err(nom::Err::Error((input, nom::error::ErrorKind::Tag)));
    }

    Ok((
        input,
        Chunk {
            length,
            chunk_type: String::from_utf8(chunk_type.to_vec()).unwrap(),
            data: data.to_vec(),
            crc,
        },
    ))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, header) = parse_png_header(input)?;
    let (input, chunks) = many0(|i| parse_chunk(i, b"IDAT"))(input)?;

    Ok((input, PngImage { header, chunks }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_png(&buffer) {
        Ok((_, png)) => {
            println!("{:#?}", png);
        }
        Err(e) => {
            eprintln!("Failed to parse PNG: {:?}", e);
        }
    }

    Ok(())
}