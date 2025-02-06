use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::many_till,
    number::complete::{be_u32, be_u8},
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
    type_: String,
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

fn parse_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    map(tag(&[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]), |sig: &[u8]| {
        sig.try_into().unwrap()
    })(input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, type_) = map(take(4usize), |t: &[u8]| String::from_utf8_lossy(t).to_string())(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;
    Ok((
        input,
        Chunk {
            length,
            type_,
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
    let (input, signature) = parse_signature(input)?;
    let (input, (chunks, _)) = many_till(parse_chunk, parse_chunk)(input)?;
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
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    match parse_png(&data) {
        Ok((_, png)) => {
            println!("{:#?}", png);
        }
        Err(e) => {
            eprintln!("Failed to parse PNG: {:?}", e);
        }
    }
}