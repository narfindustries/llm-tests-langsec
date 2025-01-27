use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::{length_data, many_till},
    number::complete::{be_u16, be_u31, be_u8},
    sequence::{tuple, preceded},
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct PngHeader {
    signature: [u8; 8],
}

impl PngHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], PngHeader> {
        map(take(8usize), |signature: &[u8]| PngHeader {
            signature: signature.try_into().expect("signature has wrong length"),
        })(input)
    }
}

#[derive(Debug)]
enum PngChunkType {
    Ihdr,
    Plte,
    Idat,
    Iend,
    Other([u8; 4]),
}

impl PngChunkType {
    fn parse(input: &[u8]) -> IResult<&[u8], PngChunkType> {
        let (input, chunk_type) = take(4usize)(input)?;
        match chunk_type {
            b"IHDR" => Ok((input, PngChunkType::Ihdr)),
            b"PLTE" => Ok((input, PngChunkType::Plte)),
            b"IDAT" => Ok((input, PngChunkType::Idat)),
            b"IEND" => Ok((input, PngChunkType::Iend)),
            _ => Ok((input, PngChunkType::Other(chunk_type.try_into().expect("wrong length")))),
        }
    }
}

#[derive(Debug)]
struct PngChunk {
    length: u32,
    chunk_type: PngChunkType,
    data: Vec<u8>,
    crc: u32,
}

impl PngChunk {
    fn parse(input: &[u8]) -> IResult<&[u8], PngChunk> {
        let (input, length) = be_u31(input)?;
        let (input, chunk_type) = PngChunkType::parse(input)?;
        let (input, data) = take(length as usize)(input)?;
        let (input, crc) = be_u32(input)?;
        Ok((
            input,
            PngChunk {
                length,
                chunk_type,
                data: data.to_vec(),
                crc,
            },
        ))
    }
}

#[derive(Debug)]
struct PngIhdrChunk {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

impl PngIhdrChunk {
    fn parse(input: &[u8]) -> IResult<&[u8], PngIhdrChunk> {
        let (input, width) = be_u32(input)?;
        let (input, height) = be_u32(input)?;
        let (input, bit_depth) = be_u8(input)?;
        let (input, color_type) = be_u8(input)?;
        let (input, compression_method) = be_u8(input)?;
        let (input, filter_method) = be_u8(input)?;
        let (input, interlace_method) = be_u8(input)?;
        Ok((
            input,
            PngIhdrChunk {
                width,
                height,
                bit_depth,
                color_type,
                compression_method,
                filter_method,
                interlace_method,
            },
        ))
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }
    let mut file = File::open(&args[1])?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    let (_, header) = PngHeader::parse(&data).expect("failed to parse header");
    println!("Header: {:?}", header);
    let mut input = &data[8..];
    let mut chunks = Vec::new();
    while input.len() > 0 {
        let (remaining, chunk) = PngChunk::parse(input).expect("failed to parse chunk");
        chunks.push(chunk);
        input = remaining;
    }
    for chunk in chunks {
        match chunk.chunk_type {
            PngChunkType::Ihdr => {
                let (_, ihdr) = PngIhdrChunk::parse(&chunk.data).expect("failed to parse ihdr chunk");
                println!("IHDR Chunk: {:?}", ihdr);
            }
            _ => println!("Chunk: {:?}", chunk),
        }
    }
    Ok(())
}