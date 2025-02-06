use nom::{
    bytes::complete::{tag, take, take_while},
    combinator::{map, value},
    multi::{many0, many1},
    number::complete::{be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, PartialEq, Clone)]
enum ColorType {
    Grayscale,
    RGB,
    Plte,
    GreyscaleAlpha,
    RGBA,
}

#[derive(Debug, PartialEq, Clone)]
enum CompressionMethod {
    DeflateInflate,
}

#[derive(Debug, PartialEq, Clone)]
enum FilterMethod {
    AdaptiveFiltering,
}

#[derive(Debug, PartialEq, Clone)]
enum InterlaceMethod {
    NoInterlace,
    Adam7Interlace,
}

#[derive(Debug, PartialEq, Clone)]
struct IhdrChunk {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: ColorType,
    compression_method: CompressionMethod,
    filter_method: FilterMethod,
    interlace_method: InterlaceMethod,
}

#[derive(Debug, PartialEq, Clone)]
struct PlteChunk {
    palette_entries: Vec<(u8, u8, u8)>,
}

#[derive(Debug, PartialEq, Clone)]
struct IdatChunk {
    compressed_image_data: Vec<u8>,
}

#[derive(Debug, PartialEq, Clone)]
struct IendChunk {}

#[derive(Debug, PartialEq, Clone)]
enum Chunk {
    Ihdr(IhdrChunk),
    Plte(PlteChunk),
    Idat(IdatChunk),
    Iend(IendChunk),
    Other(Vec<u8>),
}

fn png_file_signature(input: &[u8]) -> IResult<&[u8], ()> {
    let signature = [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a];
    let result = tag(&signature)(input);
    result.map(|(input, _)| (input, ()))
}

fn chunk_length(input: &[u8]) -> IResult<&[u8], u32> {
    be_u32(input)
}

fn chunk_type(input: &[u8]) -> IResult<&[u8], String> {
    map(take(4u8), |chunk_type: &[u8]| {
        String::from_utf8_lossy(chunk_type).into_owned()
    })(input)
}

fn chunk_crc(input: &[u8]) -> IResult<&[u8], u32> {
    be_u32(input)
}

fn ihdr_chunk(input: &[u8]) -> IResult<&[u8], IhdrChunk> {
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
            IhdrChunk {
                width,
                height,
                bit_depth,
                color_type: match color_type {
                    0 => ColorType::Grayscale,
                    2 => ColorType::RGB,
                    3 => ColorType::Plte,
                    4 => ColorType::GreyscaleAlpha,
                    6 => ColorType::RGBA,
                    _ => panic!("Invalid color type"),
                },
                compression_method: match compression_method {
                    0 => CompressionMethod::DeflateInflate,
                    _ => panic!("Invalid compression method"),
                },
                filter_method: match filter_method {
                    0 => FilterMethod::AdaptiveFiltering,
                    _ => panic!("Invalid filter method"),
                },
                interlace_method: match interlace_method {
                    0 => InterlaceMethod::NoInterlace,
                    1 => InterlaceMethod::Adam7Interlace,
                    _ => panic!("Invalid interlace method"),
                },
            }
        },
    )(input)
}

fn plte_chunk(input: &[u8]) -> IResult<&[u8], PlteChunk> {
    map(
        many1(tuple((be_u8, be_u8, be_u8))),
        |palette_entries: Vec<(u8, u8, u8)>| PlteChunk { palette_entries },
    )(input)
}

fn idat_chunk(input: &[u8]) -> IResult<&[u8], IdatChunk> {
    map(
        take_while(|x: &u8| *x != 0),
        |compressed_image_data: &[u8]| IdatChunk {
            compressed_image_data: compressed_image_data.to_vec(),
        },
    )(input)
}

fn iend_chunk(input: &[u8]) -> IResult<&[u8], IendChunk> {
    value(IendChunk {}, tag("IEND"))(input)
}

fn chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    preceded(
        tuple((chunk_length, chunk_type)),
        tuple((take_while(|x: &u8| *x != 0), chunk_crc)),
    )(input)
    .and_then(|(input, ((length, chunk_type), (chunk_data, crc)))| {
        match chunk_type.as_str() {
            "IHDR" => map(ihdr_chunk, Chunk::Ihdr)(input),
            "PLTE" => map(plte_chunk, Chunk::Plte)(input),
            "IDAT" => map(idat_chunk, Chunk::Idat)(input),
            "IEND" => map(iend_chunk, Chunk::Iend)(input),
            _ => map(take(length), |other: &[u8]| Chunk::Other(other.to_vec()))(input),
        }
    })
}

fn png_file(input: &[u8]) -> IResult<&[u8], Vec<Chunk>> {
    preceded(png_file_signature, many0(chunk))(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let result = png_file(&input);
    match result {
        Ok((_, chunks)) => println!("{:?}", chunks),
        Err(err) => panic!("{:?}", err),
    }
}