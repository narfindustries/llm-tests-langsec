use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    number::complete::be_u32,
    sequence::{tuple, preceded},
    IResult,
};

#[derive(Debug)]
struct PngHeader {
    signature: [u8; 8],
    ihdr: IHDR,
    // Add other chunks here as needed...  This is a simplified example.
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

fn png_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    map(take(8usize), |bytes: &[u8]| {
        let mut arr = [0u8; 8];
        arr.copy_from_slice(bytes);
        arr
    })(input)
}

fn chunk_length(input: &[u8]) -> IResult<&[u8], u32> {
    be_u32(input)
}

fn chunk_type(input: &[u8]) -> IResult<&[u8], [u8; 4]> {
    map(take(4usize), |bytes: &[u8]| {
        let mut arr = [0u8; 4];
        arr.copy_from_slice(bytes);
        arr
    })(input)
}

fn chunk_data(input: &[u8], len: u32) -> IResult<&[u8], Vec<u8>> {
    map(take(len as usize), |bytes: &[u8]| bytes.to_vec())(input)
}


fn chunk_crc(input: &[u8]) -> IResult<&[u8], u32> {
    be_u32(input)
}

fn ihdr(input: &[u8]) -> IResult<&[u8], IHDR> {
    map(
        tuple((
            be_u32,
            be_u32,
            map(take(1usize), |b| b[0]),
            map(take(1usize), |b| b[0]),
            map(take(1usize), |b| b[0]),
            map(take(1usize), |b| b[0]),
            map(take(1usize), |b| b[0]),
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
    )(input)
}

fn png_chunk(input: &[u8]) -> IResult<&[u8], ([u8;4],Vec<u8>)> {
    let (input, len) = chunk_length(input)?;
    let (input, type_) = chunk_type(input)?;
    let (input, data) = chunk_data(input, len)?;
    let (input, _) = chunk_crc(input)?;
    Ok((input, (type_,data)))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngHeader> {
    let (input, signature) = png_signature(input)?;
    let (input, ihdr) = preceded(png_chunk, ihdr)(input)?;

    Ok((
        input,
        PngHeader {
            signature,
            ihdr,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: png_parser <filename>");
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    match parse_png(&buffer) {
        Ok((_, header)) => println!("PNG Header: {:?}", header),
        Err(err) => println!("Error parsing PNG: {:?}", err),
    }
}
