use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::many0,
    number::complete::{be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs, path::Path};

const PNG_SIGNATURE: [u8; 8] = [137, 80, 78, 71, 13, 10, 26, 10];

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
    map(take(8usize), |bytes: &[u8]| bytes.try_into().unwrap())(input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = map(take(4usize), |bytes: &[u8]| bytes.try_into().unwrap())(input)?;
    let (input, chunk_data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;
    
    Ok((input, Chunk {
        length,
        chunk_type,
        chunk_data: chunk_data.to_vec(),
        crc,
    }))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PNG> {
    let (input, signature) = verify(parse_signature, |sig| sig == &PNG_SIGNATURE)(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;
    
    Ok((input, PNG { signature, chunks }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let data = fs::read(path)?;
    
    match parse_png(&data) {
        Ok((remaining, png)) => {
            if !remaining.is_empty() {
                println!("Warning: {} bytes of unparsed data remaining", remaining.len());
            }
            println!("{:#?}", png);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse PNG: {:?}", e);
            std::process::exit(1);
        }
    }
}