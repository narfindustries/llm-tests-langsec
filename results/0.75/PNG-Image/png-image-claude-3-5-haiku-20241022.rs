use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::count,
    number::complete::{be_u8, be_u16, be_u32},
    sequence::tuple,
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
struct PngImage {
    signature: [u8; 8],
    chunks: Vec<PngChunk>,
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    let signature = [137, 80, 78, 71, 13, 10, 26, 10];
    map(tag(&signature[..]), |_| signature)(input)
}

fn parse_png_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
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

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, signature) = parse_png_signature(input)?;
    let (input, chunks) = many0(parse_png_chunk)(input)?;

    Ok((input, PngImage {
        signature,
        chunks,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_png(&buffer) {
        Ok((_, png)) => {
            println!("PNG Image parsed successfully:");
            println!("{:?}", png);
        }
        Err(err) => {
            eprintln!("Failed to parse PNG: {:?}", err);
            std::process::exit(1);
        }
    }
}