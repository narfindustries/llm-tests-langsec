use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct PNG {
    header: PNGHeader,
    chunks: Vec<Chunk>,
}

#[derive(Debug)]
struct PNGHeader {
    signature: [u8; 8],
}

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: [u8; 4],
    data: Vec<u8>,
    crc: u32,
}

fn parse_png_header(input: &[u8]) -> IResult<&[u8], PNGHeader> {
    let (input, signature) = take(8usize)(input)?;
    Ok((input, PNGHeader { signature: signature.try_into().unwrap() }))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, (length, chunk_type, data, crc)) = tuple((
        be_u32,
        take(4usize),
        take(be_u32),
        be_u32,
    ))(input)?;
    Ok((input, Chunk {
        length,
        chunk_type: chunk_type.try_into().unwrap(),
        data: data.to_vec(),
        crc,
    }))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PNG> {
    let (input, header) = parse_png_header(input)?;
    let mut chunks = Vec::new();
    let mut remaining_input = input;
    while !remaining_input.is_empty() {
        let (input, chunk) = parse_chunk(remaining_input)?;
        chunks.push(chunk);
        remaining_input = input;
    }
    Ok((remaining_input, PNG { header, chunks }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_png(&buffer) {
        Ok((_, png)) => println!("{:#?}", png),
        Err(e) => eprintln!("Failed to parse PNG: {:?}", e),
    }

    Ok(())
}