use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::many0,
    number::complete::{be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct PNG {
    signature: [u8; 8],
    chunks: Vec<Chunk>,
}

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: [u8; 4],
    chunk_data: Vec<u8>,
    crc: u32,
}

fn parse_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    map(
        verify(take(8usize), |b: &[u8]| {
            b == &[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
        }),
        |b: &[u8]| b.try_into().unwrap(),
    )(input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = map(take(4usize), |b: &[u8]| b.try_into().unwrap())(input)?;
    let (input, chunk_data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;

    Ok((
        input,
        Chunk {
            length,
            chunk_type,
            chunk_data: chunk_data.to_vec(),
            crc,
        },
    ))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PNG> {
    map(
        tuple((parse_signature, many0(parse_chunk))),
        |(signature, chunks)| PNG { signature, chunks },
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
        Ok((_, png)) => println!("{:#?}", png),
        Err(e) => eprintln!("Error parsing PNG: {:?}", e),
    }

    Ok(())
}