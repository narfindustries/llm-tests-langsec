use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, verify},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{tuple, preceded},
    IResult,
};

#[derive(Debug)]
struct PngHeader {
    signature: [u8; 8],
}

#[derive(Debug)]
struct PngChunk {
    length: u32,
    type_: [u8; 4],
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct PngIHDR {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

#[derive(Debug)]
struct PngImage {
    header: PngHeader,
    chunks: Vec<PngChunk>,
}


fn png_signature(input: &[u8]) -> IResult<&[u8], PngHeader> {
    map(tag(b"\x89PNG\r\n\x1a\n"), |_| PngHeader { signature: [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a] })(input)
}

fn png_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, length) = be_u32(input)?;
    let (input, type_) = take(4usize)(input)?;
    let (input, data) = take(length as usize)(input)?;
    let (input, crc) = be_u32(input)?;
    Ok((input, PngChunk { length, type_: type_.try_into().unwrap(), data: data.to_vec(), crc }))
}

fn png_ihdr(input: &[u8]) -> IResult<&[u8], PngIHDR> {
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) = tuple((be_u32, be_u32, be_u8, be_u8, be_u8, be_u8, be_u8))(input)?;
    Ok((input, PngIHDR { width, height, bit_depth, color_type, compression_method, filter_method, interlace_method }))
}

fn png_image(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, header) = png_signature(input)?;
    let (input, chunks) = many0(preceded(tag(b""), png_chunk))(input)?;
    Ok((input, PngImage { header, chunks }))
}

fn many0<I, O, E, F>(f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    F: FnMut(I) -> IResult<I, O, E>,
    I: Clone,
{
    move |i| {
        let mut acc = Vec::new();
        let mut input = i.clone();
        loop {
            match f(input.clone()) {
                Ok((i, o)) => {
                    acc.push(o);
                    input = i;
                }
                Err(nom::Err::Error(_)) | Err(nom::Err::Failure(_)) => break,
                Err(nom::Err::Incomplete(_)) => return Err(nom::Err::Incomplete(_)),
            }
        }
        Ok((input, acc))
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    };

    match png_image(&buffer) {
        Ok((_, image)) => println!("{:#?}", image),
        Err(err) => eprintln!("Error parsing PNG: {:?}", err),
    }
}
