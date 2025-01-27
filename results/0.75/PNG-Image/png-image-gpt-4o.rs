use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::tuple,
    combinator::{map_res, opt},
    multi::many1,
};

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: String,
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct PngImage {
    header: [u8; 8],
    chunks: Vec<Chunk>,
}

fn parse_png_header(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    let (input, header) = take(8u8)(input)?;
    let header: &[u8; 8] = header.try_into().expect("slice with incorrect length");
    Ok((input, *header))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, (length, chunk_type, data, crc)) = tuple((
        be_u32,
        map_res(take(4u8), |s: &[u8]| std::str::from_utf8(s).map(String::from)),
        |input, length| take(length)(input),
        be_u32,
    ))(input)?;

    Ok((input, Chunk { length, chunk_type, data: data.to_vec(), crc }))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, header) = parse_png_header(input)?;
    let (input, chunks) = many1(parse_chunk)(input)?;

    Ok((input, PngImage { header, chunks }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_png_file>", args[0]);
        std::process::exit(1);
    }

    let input_path = Path::new(&args[1]);
    let mut file = File::open(input_path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_png(&buffer) {
        Ok((_, png_image)) => {
            println!("{:?}", png_image);
        }
        Err(e) => {
            eprintln!("Failed to parse PNG file: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}