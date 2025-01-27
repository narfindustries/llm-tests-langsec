use nom::{
    bytes::complete::take,
    number::complete::{be_u32, be_u8},
    IResult, multi::count,
};
use std::{env, fs::File, io::Read, path::Path};

/// Represents the PNG signature/magic number
const PNG_SIGNATURE: [u8; 8] = [137, 80, 78, 71, 13, 10, 26, 10];

/// Parse the PNG Signature
fn parse_png_signature(input: &[u8]) -> IResult<&[u8], &[u8]> {
    nom::bytes::complete::tag(PNG_SIGNATURE)(input)
}

/// Represents a PNG chunk
#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: String,
    data: Vec<u8>,
    crc: u32,
}

/// Parse a single PNG chunk
fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;

    let chunk_type = String::from_utf8(chunk_type.to_vec()).unwrap();

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

/// Parse multiple PNG chunks
fn parse_chunks(input: &[u8]) -> IResult<&[u8], Vec<Chunk>> {
    let mut result = Vec::new();
    let mut input = input;
    while input.len() > 0 {
        let (next_input, chunk) = parse_chunk(input)?;
        input = next_input;
        result.push(chunk);
        if chunk.chunk_type == "IEND" {
            break;
        }
    }
    Ok((input, result))
}

/// Represents the whole PNG file format
#[derive(Debug)]
struct PngImage {
    signature: Vec<u8>,
    chunks: Vec<Chunk>,
}

/// Parse entire PNG image
fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, signature) = parse_png_signature(input)?;
    let (input, chunks) = parse_chunks(input)?;

    Ok((
        input,
        PngImage {
            signature: signature.to_vec(),
            chunks,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <PNG file path>", args[0]);
        return;
    }
    let path = &args[1];
    let mut file = File::open(Path::new(path)).expect("Unable to open file");
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Unable to read file");

    match parse_png(&data) {
        Ok((_rest, png)) => println!("{:#?}", png),
        Err(e) => eprintln!("Error parsing PNG: {:?}", e),
    }
}