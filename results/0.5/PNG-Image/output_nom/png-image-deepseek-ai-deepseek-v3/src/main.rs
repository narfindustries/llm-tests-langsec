use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::{length_data, many_till},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct PNGFile {
    signature: [u8; 8],
    chunks: Vec<Chunk>,
}

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: [u8; 4],
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

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    let (input, sig) = take(8u8)(input)?;
    Ok((input, sig.try_into().unwrap()))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4u8)(input)?;
    let (input, data) = length_data(map(be_u32, |len| len as usize))(input)?;
    let (input, crc) = be_u32(input)?;
    Ok((
        input,
        Chunk {
            length,
            chunk_type: chunk_type.try_into().unwrap(),
            data: data.to_vec(),
            crc,
        },
    ))
}

fn parse_ihdr(input: &[u8]) -> IResult<&[u8], IHDR> {
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) =
        tuple((be_u32, be_u32, be_u8, be_u8, be_u8, be_u8, be_u8))(input)?;
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
    let (input, (chunks, _)) = many_till(parse_chunk, tag(b"IEND"))(input)?;
    Ok((input, PNGFile { signature, chunks }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    match parse_png(&data) {
        Ok((_, png_file)) => {
            println!("{:#?}", png_file);
        }
        Err(e) => {
            eprintln!("Failed to parse PNG: {:?}", e);
        }
    }
}