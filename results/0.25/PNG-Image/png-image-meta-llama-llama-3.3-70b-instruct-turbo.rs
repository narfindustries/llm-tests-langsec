use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    error::{context, ErrorKind, ParseError},
    multi::{length_data, take_while_m_n},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{tuple, preceded},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, Result},
    path::Path,
};

#[derive(Debug)]
struct IHDR {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

impl IHDR {
    fn parse(input: &[u8]) -> IResult<&[u8], IHDR> {
        context(
            "IHDR",
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
                |(width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)| IHDR {
                    width,
                    height,
                    bit_depth,
                    color_type,
                    compression_method,
                    filter_method,
                    interlace_method,
                },
            ),
        )(input)
    }
}

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: [u8; 4],
    data: Vec<u8>,
    crc: u32,
}

impl Chunk {
    fn parse(input: &[u8]) -> IResult<&[u8], Chunk> {
        context(
            "Chunk",
            map(
                tuple((
                    be_u32,
                    take(4u8),
                    length_data(be_u32),
                    be_u32,
                )),
                |(length, chunk_type, data, crc)| Chunk {
                    length,
                    chunk_type,
                    data,
                    crc,
                },
            ),
        )(input)
    }
}

#[derive(Debug)]
struct PNG {
    signature: [u8; 8],
    ihdr: IHDR,
    chunks: Vec<Chunk>,
}

impl PNG {
    fn parse(input: &[u8]) -> IResult<&[u8], PNG> {
        context(
            "PNG",
            map(
                tuple((
                    tag(&[137, 80, 78, 71, 13, 10, 26, 10]),
                    IHDR::parse,
                    take_while_m_n(
                        1,
                        usize::MAX,
                        |input| {
                            let (input, chunk) = Chunk::parse(input).unwrap();
                            chunk.chunk_type != [73, 68, 65, 84] && chunk.chunk_type != [74, 84, 69, 88]
                        },
                    ),
                )),
                |(signature, ihdr, chunks)| PNG { signature, ihdr, chunks },
            ),
        )(input)
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let input_file = &args[1];
    let mut file = File::open(input_file)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    let (_, png) = PNG::parse(&data).unwrap();
    println!("{:?}", png);
    Ok(())
}