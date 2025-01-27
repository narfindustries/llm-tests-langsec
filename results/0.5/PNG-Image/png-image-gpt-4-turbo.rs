use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::env;
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

fn parse_png_header(input: &[u8]) -> IResult<&[u8], PngHeader> {
    let (input, _) = tag(b"\x89PNG\r\n\x1a\n")(input)?;
    let (input, _) = parse_chunk(input, b"IHDR")?;
    let (input, width) = be_u32(input)?;
    let (input, height) = be_u32(input)?;
    let (input, bit_depth) = nom::number::complete::u8(input)?;
    let (input, color_type) = nom::number::complete::u8(input)?;
    let (input, compression_method) = nom::number::complete::u8(input)?;
    let (input, filter_method) = nom::number::complete::u8(input)?;
    let (input, interlace_method) = nom::number::complete::u8(input)?;

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

fn parse_chunk(input: &[u8], expected_type: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;

    if &chunk_type != expected_type {
        return Err(nom::Err::Failure(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }

    Ok((
        input,
        Chunk {
            length,
            chunk_type: String::from_utf8(chunk_type.to_vec()).unwrap(),
            data: data.to_vec(),
            crc,
        },
    ))
}

fn read_file_contents(filename: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(filename)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    Ok(data)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <PNG file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    match read_file_contents(filename) {
        Ok(bytes) => match parse_png_header(&bytes) {
            Ok((_, header)) => {
                println!("Parsed PNG header: {:?}", header);
            }
            Err(e) => {
                eprintln!("Error parsing PNG header: {:?}", e);
            }
        },
        Err(e) => {
            eprintln!("Failed to read '{}': {:?}", filename, e);
        }
    }
}