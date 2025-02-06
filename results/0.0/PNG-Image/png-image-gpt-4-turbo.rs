use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::{count, many0},
    number::complete::{be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{fs::File, io::{self, Read}, path::PathBuf};
use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Opt {
    #[clap(parse(from_os_str))]
    input_file: PathBuf,
}

#[derive(Debug)]
struct Png {
    signature: Vec<u8>,
    chunks: Vec<Chunk>,
}

#[derive(Debug)]
enum Chunk {
    IHDR(IHDRChunk),
    PLTE(Vec<RGB>),
    IDAT(Vec<u8>),
    IEND,
    Other(String, Vec<u8>),
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
struct RGB {
    red: u8,
    green: u8,
    blue: u8,
}

fn parse_png(input: &[u8]) -> IResult<&[u8], Png> {
    let (input, signature) = tag(b"\x89PNG\r\n\x1a\n")(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;
    Ok((input, Png { signature: signature.to_vec(), chunks }))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, (length, chunk_type)) = tuple((be_u32, take(4usize)))(input)?;
    let (input, data) = take(length)(input)?;
    let (input, _crc) = take(4usize)(input)?;

    match chunk_type {
        b"IHDR" => map(parse_ihdr_chunk, Chunk::IHDR)(data),
        b"PLTE" => map(parse_plte_chunk, Chunk::PLTE)(data),
        b"IDAT" => Ok((input, Chunk::IDAT(data.to_vec()))),
        b"IEND" => Ok((input, Chunk::IEND)),
        _ => Ok((input, Chunk::Other(String::from_utf8_lossy(chunk_type).to_string(), data.to_vec()))),
    }
}

fn parse_ihdr_chunk(input: &[u8]) -> IResult<&[u8], IHDRChunk> {
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) =
        tuple((be_u32, be_u32, be_u8, be_u8, be_u8, be_u8, be_u8))(input)?;
    Ok((
        input,
        IHDRChunk {
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

fn parse_plte_chunk(input: &[u8]) -> IResult<&[u8], Vec<RGB>> {
    let num_entries = input.len() / 3;
    count(map(tuple((be_u8, be_u8, be_u8)), |(r, g, b)| RGB { red: r, green: g, blue: b }), num_entries)(input)
}

fn main() -> io::Result<()> {
    let opt = Opt::parse();
    let mut file = File::open(opt.input_file)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_png(&buffer) {
        Ok((_, png)) => println!("{:#?}", png),
        Err(e) => println!("Failed to parse PNG: {:?}", e),
    }

    Ok(())
}