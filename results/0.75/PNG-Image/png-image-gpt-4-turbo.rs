use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{count, many0},
    number::complete::{be_u16, be_u32},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Cli {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}

#[derive(Debug, PartialEq)]
struct PngHeader {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

#[derive(Debug, PartialEq)]
struct Chunk {
    length: u32,
    chunk_type: String,
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug, PartialEq)]
struct PngImage {
    header: PngHeader,
    chunks: Vec<Chunk>,
}

fn parse_png_header(input: &[u8]) -> IResult<&[u8], PngHeader> {
    let (input, _) = tag(b"\x89PNG\r\n\x1a\n")(input)?;
    let (input, _) = parse_chunk_type(input, b"IHDR")?;
    let (
        input,
        (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method),
    ) = tuple((be_u32, be_u32, nom::number::complete::u8, nom::number::complete::u8, nom::number::complete::u8, nom::number::complete::u8, nom::number::complete::u8))(input)?;
    Ok((
        input,
        PngHeader {
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

fn parse_chunk_type(input: &[u8], expected_type: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(expected_type)(input)?;
    Ok((input, ()))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = map_res(take(4usize), |s: &[u8]| std::str::from_utf8(s))(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;
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

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, header) = parse_png_header(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;
    Ok((input, PngImage { header, chunks }))
}

fn main() -> io::Result<()> {
    let args = Cli::from_args();
    let mut file = File::open(args.input)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_png(&buffer) {
        Ok((_, png)) => println!("{:#?}", png),
        Err(e) => eprintln!("Failed to parse PNG: {:?}", e),
    }

    Ok(())
}