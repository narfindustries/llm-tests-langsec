use std::env;
use std::fs;
use std::error::Error;
use nom::{
    IResult,
    bytes::complete::{take, tag},
    number::complete::{u32, u16},
    combinator::{map, map_res, all_consuming},
    sequence::tuple,
    multi::count,
};

#[derive(Debug)]
struct PngHeader {
    signature: [u8; 8],
}

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

#[derive(Debug)]
struct Chunk {
    length: u32,
    type_: [u8; 4],
    data: Vec<u8>,
    crc: u32,
}


fn png_signature(input: &[u8]) -> IResult<&[u8], PngHeader> {
    map(tag(b"\x89PNG\r\n\x1a\n"), |_| PngHeader { signature: *b"\x89PNG\r\n\x1a\n" })(input)
}

fn ihdr(input: &[u8]) -> IResult<&[u8], IHDR> {
    map(tuple((u32, u32, u8, u8, u8, u8, u8)), |(width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)| IHDR {
        width,
        height,
        bit_depth,
        color_type,
        compression_method,
        filter_method,
        interlace_method,
    })(input)
}

fn chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    map(tuple((u32, take(4usize), |i| take(i)(i), u32)), |(length, type_, data, crc)| Chunk {
        length,
        type_: type_.try_into().unwrap(),
        data: data.to_vec(),
        crc,
    })(input)
}

fn png(input: &[u8]) -> IResult<&[u8], (PngHeader, Vec<Chunk>)> {
    let (input, header) = png_signature(input)?;
    let (input, chunks) = many0(chunk)(input)?;
    Ok((input, (header, chunks)))
}

fn many0<I, O, E: nom::error::ParseError<I>>(f: impl Fn(I) -> IResult<I, O, E>) -> impl Fn(I) -> IResult<I, Vec<O>, E> {
    nom::multi::many0(f)
}


fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: png_parser <filename>");
        return Ok(());
    }

    let filename = &args[1];
    let file_contents = fs::read(filename)?;


    match all_consuming(png)(&file_contents) {
        Ok((_, (header, chunks))) => {
            println!("PNG Header: {:?}", header);
            for chunk in chunks {
                println!("Chunk: {:?}", chunk);
            }
        }
        Err(e) => {
            println!("Error parsing PNG: {}", e);
        }
    }

    Ok(())
}
