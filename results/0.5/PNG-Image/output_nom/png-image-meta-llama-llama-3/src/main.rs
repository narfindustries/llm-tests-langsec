use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{length_data, many_till},
    sequence::{pair, tuple},
    IResult,
};
use std::{
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

#[derive(Debug)]
struct PngHeader {
    magic: [u8; 8],
}

#[derive(Debug)]
struct PngChunk {
    length: u32,
    chunk_type: [u8; 4],
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct IhdrChunk {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

#[derive(Debug)]
struct PlteChunk {
    palette: Vec<[u8; 3]>,
}

#[derive(Debug)]
struct IdatChunk {
    data: Vec<u8>,
}

#[derive(Debug)]
struct IendChunk {}

fn png_header(input: &[u8]) -> IResult<&[u8], PngHeader> {
    map(tag(&[137, 80, 78, 71, 13, 10, 26, 10]), |magic: &[u8]| {
        PngHeader { magic: magic.try_into().unwrap() }
    })(input)
}

fn png_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, (length, chunk_type)) = pair(u32, take(4usize))(input)?;
    let (input, data) = length_data(get_u32_be)(input)?;
    let (input, crc) = u32(input)?;
    Ok((
        input,
        PngChunk {
            length,
            chunk_type: chunk_type.try_into().unwrap(),
            data,
            crc,
        },
    ))
}

fn ihdr_chunk(input: &[u8]) -> IResult<&[u8], IhdrChunk> {
    map(
        tuple((u32, u32, u8, u8, u8, u8, u8)),
        |(width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)| {
            IhdrChunk {
                width,
                height,
                bit_depth,
                color_type,
                compression_method,
                filter_method,
                interlace_method,
            }
        },
    )(input)
}

fn plte_chunk(input: &[u8]) -> IResult<&[u8], PlteChunk> {
    map(
        many_till(take(3usize), tag(&[0, 0, 0, 0])),
        |palette: Vec<_>| PlteChunk { palette },
    )(input)
}

fn idat_chunk(input: &[u8]) -> IResult<&[u8], IdatChunk> {
    map(take_till(|x| x == 0), |data| IdatChunk { data })(
        input,
    )
}

fn iend_chunk(input: &[u8]) -> IResult<&[u8], IendChunk> {
    value(IendChunk {}, tag(&[0, 0, 0, 0]))(input)
}

fn get_u32_be(input: &[u8]) -> IResult<&[u8], u32> {
    let (input, bytes) = take(4usize)(input)?;
    let value = ((bytes[0] as u32) << 24)
        | ((bytes[1] as u32) << 16)
        | ((bytes[2] as u32) << 8)
        | (bytes[3] as u32);
    Ok((input, value))
}

fn main() -> std::io::Result<()> {
    let path = std::env::args().nth(1).expect("please provide a file path");
    let path = Path::new(&path);
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input)?;
    let (input, header) = png_header(&input).unwrap();
    println!("PNG Header: {:?}", header);
    let mut chunks: Vec<PngChunk> = Vec::new();
    while !input.is_empty() {
        let (new_input, chunk) = png_chunk(&input).unwrap();
        input = new_input;
        chunks.push(chunk);
        match chunk.chunk_type {
            b"IHDR" => {
                let (_, ihdr) = ihdr_chunk(&chunk.data).unwrap();
                println!("IHDR Chunk: {:?}", ihdr);
            }
            b"PLTE" => {
                let (_, plte) = plte_chunk(&chunk.data).unwrap();
                println!("PLTE Chunk: {:?}", plte);
            }
            b"IDAT" => {
                let (_, idat) = idat_chunk(&chunk.data).unwrap();
                println!("IDAT Chunk: {:?}", idat);
            }
            b"IEND" => {
                let (_, iend) = iend_chunk(&chunk.data).unwrap();
                println!("IEND Chunk: {:?}", iend);
            }
            _ => {}
        }
    }
    Ok(())
}