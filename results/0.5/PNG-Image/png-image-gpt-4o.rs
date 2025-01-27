// Cargo.toml
// [dependencies]
// nom = "7.1.1"
// clap = { version = "4.0", features = ["derive"] }

use clap::Parser;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::many_till,
    number::complete::{be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

#[derive(Parser)]
struct Cli {
    input: String,
}

#[derive(Debug)]
struct PngChunk {
    length: u32,
    chunk_type: [u8; 4],
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct PngFile {
    signature: [u8; 8],
    chunks: Vec<PngChunk>,
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    map(tag(&[137, 80, 78, 71, 13, 10, 26, 10]), |s| {
        [s[0], s[1], s[2], s[3], s[4], s[5], s[6], s[7]]
    })(input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    map(
        tuple((
            be_u32,
            map_res(take(4usize), |s: &[u8]| Ok([s[0], s[1], s[2], s[3]])),
            |i: &[u8]| {
                let (i, length) = be_u32(i)?;
                take(length as usize)(i)
            },
            be_u32,
        )),
        |(length, chunk_type, data, crc)| PngChunk {
            length,
            chunk_type,
            data: data.to_vec(),
            crc,
        },
    )(input)
}

fn parse_png_chunks(input: &[u8]) -> IResult<&[u8], Vec<PngChunk>> {
    many_till(parse_chunk, tag("IEND"))(input).map(|(i, (chunks, _))| (i, chunks))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngFile> {
    map(
        tuple((parse_png_signature, parse_png_chunks)),
        |(signature, chunks)| PngFile { signature, chunks },
    )(input)
}

fn main() -> io::Result<()> {
    let args = Cli::parse();
    let path = Path::new(&args.input);
    let mut file = File::open(&path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_png(&buffer) {
        Ok((_, png)) => {
            println!("Parsed PNG: {:?}", png);
        }
        Err(e) => {
            eprintln!("Failed to parse PNG: {:?}", e);
        }
    }

    Ok(())
}