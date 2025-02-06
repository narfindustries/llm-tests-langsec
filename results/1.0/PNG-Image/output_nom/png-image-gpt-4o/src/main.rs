use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult, bytes::complete::take, number::complete::{be_u8, be_u32},
};

#[derive(Debug)]
struct PngChunk {
    length: u32,
    chunk_type: [u8; 4],
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct PngFile {
    header: [u8; 8],
    chunks: Vec<PngChunk>,
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    let (input, signature) = take(8usize)(input)?;
    let mut sig_array = [0; 8];
    sig_array.copy_from_slice(signature);
    Ok((input, sig_array))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let mut type_array = [0; 4];
    type_array.copy_from_slice(chunk_type);
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;
    Ok((input, PngChunk { length, chunk_type: type_array, data: data.to_vec(), crc }))
}

fn parse_chunks(input: &[u8]) -> IResult<&[u8], Vec<PngChunk>> {
    let mut chunks = Vec::new();
    let mut remaining = input;
    while !remaining.is_empty() {
        let (new_remaining, chunk) = parse_chunk(remaining)?;
        chunks.push(chunk);
        remaining = new_remaining;
    }
    Ok((remaining, chunks))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngFile> {
    let (input, header) = parse_png_signature(input)?;
    let (input, chunks) = parse_chunks(input)?;
    Ok((input, PngFile { header, chunks }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let mut file = File::open(file_path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_png(&buffer) {
        Ok((_, png)) => {
            println!("{:#?}", png);
        },
        Err(e) => {
            eprintln!("Failed to parse PNG: {:?}", e);
        }
    }
}