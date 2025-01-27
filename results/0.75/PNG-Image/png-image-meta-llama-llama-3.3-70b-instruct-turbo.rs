use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map, map_res},
    error::{Error, ErrorKind},
    multi::{many0, many1},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, Result},
    path::Path,
};

#[derive(Debug)]
enum ChunkType {
    IHDR,
    PLTE,
    IDAT,
    IEND,
    Other(Vec<u8>),
}

#[derive(Debug)]
struct IHDRChunk {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

#[derive(Debug)]
struct PLTEChunk {
    palette: Vec<(u8, u8, u8)>,
}

#[derive(Debug)]
struct IDATChunk {
    data: Vec<u8>,
}

#[derive(Debug)]
struct IENDChunk {}

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: ChunkType,
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct PNGImage {
    signature: Vec<u8>,
    chunks: Vec<Chunk>,
}

fn chunk_type(input: &[u8]) -> IResult<&[u8], ChunkType> {
    map_res(take(4usize), |input: &[u8]| {
        match input {
            b"IHDR" => Ok(ChunkType::IHDR),
            b"PLTE" => Ok(ChunkType::PLTE),
            b"IDAT" => Ok(ChunkType::IDAT),
            b"IEND" => Ok(ChunkType::IEND),
            _ => Ok(ChunkType::Other(input.to_vec())),
        }
    })(input)
}

fn ihdr_chunk(input: &[u8]) -> IResult<&[u8], IHDRChunk> {
    map(
        tuple((
            be_u32,
            be_u32,
            be_u8,
            be_u8,
            be_u8,
            be_u8,
            be_u8,
        )),
        |(width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)| {
            IHDRChunk {
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

fn plte_chunk(input: &[u8]) -> IResult<&[u8], PLTEChunk> {
    map(
        many1(map(take(3usize), |input: &[u8]| (input[0], input[1], input[2]))),
        |palette| PLTEChunk { palette },
    )(input)
}

fn idat_chunk(input: &[u8]) -> IResult<&[u8], IDATChunk> {
    map(take_while_m_n(0, 2^31 - 1, |x| true), |data| IDATChunk { data: data.to_vec() })(
        input,
    )
}

fn iend_chunk(input: &[u8]) -> IResult<&[u8], IENDChunk> {
    map(take(0usize), |_| IENDChunk {})(input)
}

fn chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    map(
        tuple((
            be_u32,
            chunk_type,
            preceded(be_u32, |input| {
                map(
                    take_while_m_n(0, 2^31 - 1, |x| true),
                    |data| match input {
                        ChunkType::IHDR => map_res(ihdr_chunk(data), |x| Ok((x, data)))?,
                        ChunkType::PLTE => map_res(plte_chunk(data), |x| Ok((x, data)))?,
                        ChunkType::IDAT => map_res(idat_chunk(data), |x| Ok((x, data)))?,
                        ChunkType::IEND => map_res(iend_chunk(data), |x| Ok((x, data)))?,
                        ChunkType::Other(_) => Ok(((), data)),
                    },
                )
            }),
            be_u32,
        ]),
        |(length, chunk_type, (data, _), crc)| Chunk {
            length,
            chunk_type,
            data: data.to_vec(),
            crc,
        },
    )(input)
}

fn png_image(input: &[u8]) -> IResult<&[u8], PNGImage> {
    map(
        tuple((
            take(8usize),
            many0(delimited(take(4usize), chunk, take(4usize))),
        )),
        |(signature, chunks)| PNGImage { signature: signature.to_vec(), chunks },
    )(input)
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }
    let input_file = &args[1];
    let mut file = File::open(Path::new(input_file))?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    match png_image(&buffer) {
        Ok((remaining, png_image)) => {
            println!("{:?}", png_image);
        }
        Err(err) => match err {
            Error::Incomplete(_) => {
                eprintln!("Error: Input data is incomplete.");
            }
            Error::Error(err) => {
                eprintln!("Error: {}", err);
            }
            Error::Failure(err) => {
                eprintln!("Error: {}", err);
            }
        },
    }
    Ok(())
}