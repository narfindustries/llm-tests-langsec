use std::fs::File;
use std::io::Read;
use std::path::Path;
use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::{be_u32, be_u8},
    combinator::map_res,
    multi::many0,
    sequence::tuple,
};

#[derive(Debug)]
struct PngChunk {
    length: u32,
    chunk_type: [u8; 4],
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct PngImage {
    signature: [u8; 8],
    chunks: Vec<PngChunk>,
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    let (input, signature) = tag(b"\x89PNG\r\n\x1a\n")(input)?;
    Ok((input, *array_ref!(signature, 0, 8)))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, (length, chunk_type, data, crc)) = tuple((
        be_u32,
        map_res(take(4usize), |bytes: &[u8]| -> Result<[u8; 4], ()> {
            Ok(*array_ref!(bytes, 0, 4))
        }),
        |input: &[u8]| {
            let (input, length) = be_u32(input)?;
            take(length as usize)(input)
        },
        be_u32,
    ))(input)?;

    Ok((input, PngChunk {
        length,
        chunk_type,
        data: data.to_vec(),
        crc,
    }))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, signature) = parse_png_signature(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;

    Ok((input, PngImage {
        signature,
        chunks,
    }))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_png(&buffer) {
        Ok((_, png)) => println!("{:?}", png),
        Err(e) => eprintln!("Failed to parse PNG: {:?}", e),
    }
}