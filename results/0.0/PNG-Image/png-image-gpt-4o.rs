use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::be_u32,
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::env;

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
    let (input, signature) = take(8usize)(input)?;
    let mut sig_array = [0u8; 8];
    sig_array.copy_from_slice(signature);
    Ok((input, sig_array))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, (length, chunk_type, data, crc)) = tuple((
        be_u32,
        map_res(take(4usize), |s: &[u8]| std::str::from_utf8(s).map(String::from)),
        take(be_u32(input)?.1 as usize),
        be_u32,
    ))(input)?;
    Ok((input, PngChunk { length, chunk_type, data: data.to_vec(), crc }))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, signature) = parse_png_signature(input)?;
    let mut chunks = Vec::new();
    let mut input = input;
    while !input.is_empty() {
        let (new_input, chunk) = parse_chunk(input)?;
        input = new_input;
        chunks.push(chunk);
    }
    Ok((input, PngImage { signature, chunks }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    if let Err(err) = file.read_to_end(&mut buffer) {
        eprintln!("Error reading file: {}", err);
        return;
    }

    match parse_png(&buffer) {
        Ok((_, png)) => println!("{:#?}", png),
        Err(err) => eprintln!("Error parsing PNG: {:?}", err),
    }
}