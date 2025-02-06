use nom::{
    bytes::complete::{tag, take},
    combinator::{map_res},
    number::complete::{be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct PngHeader {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: String,
    data: Vec<u8>,
    crc: u32,
}

fn parse_u32(input: &[u8]) -> IResult<&[u8], u32> {
    be_u32(input)
}

fn parse_u8(input: &[u8]) -> IResult<&[u8], u8> {
    be_u8(input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = parse_u32(input)?;
    let (input, chunk_type) = map_res(take(4usize), std::str::from_utf8)(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = parse_u32(input)?;

    Ok((
        input,
        Chunk {
            length,
            chunk_type: chunk_type.to_string(),
            data: data.to_vec(),
            crc,
        },
    ))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], (PngHeader, Vec<Chunk>)> {
    let (input, _) = tag(b"\x89PNG\r\n\x1a\n")(input)?;
    let (input, header_chunk) = preceded(tag(b"IHDR"), parse_chunk)(input)?;
    let header_data = header_chunk.data.clone();
    let (header_input, _) = take(0usize)(&header_data)?;
    let (_, header) = tuple((
        parse_u32,
        parse_u32,
        parse_u8,
        parse_u8,
        parse_u8,
        parse_u8,
        parse_u8,
    ))(header_input)?;

    let mut remaining = input;
    let mut chunks = Vec::new();

    while !remaining.is_empty() {
        let (new_remaining, chunk) = parse_chunk(remaining)?;
        if chunk.chunk_type == "IEND" {
            chunks.push(chunk);
            break;
        }
        chunks.push(chunk);
        remaining = new_remaining;
    }

    Ok((
        remaining,
        (
            PngHeader {
                width: header.0,
                height: header.1,
                bit_depth: header.2,
                color_type: header.3,
                compression_method: header.4,
                filter_method: header.5,
                interlace_method: header.6,
            },
            chunks,
        ),
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_png(&buffer) {
        Ok((_, (header, chunks))) => {
            println!("Header: {:?}", header);
            for chunk in chunks {
                println!("Chunk: {:?}", chunk);
            }
        }
        Err(e) => println!("Error parsing PNG: {:?}", e),
    }

    Ok(())
}