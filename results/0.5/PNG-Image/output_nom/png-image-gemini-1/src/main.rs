use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    error::ErrorKind,
    multi::count,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{preceded, tuple},
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


fn png_header(input: &[u8]) -> IResult<&[u8], PngHeader> {
    map(tag(b"\x89PNG\r\n\x1a\n"), |_| PngHeader { signature: [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a] })(input)
}

fn png_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, length) = be_u32(input)?;
    let (input, type_) = take(4usize)(input)?;
    let (input, data) = take(length as usize)(input)?;
    let (input, crc) = be_u32(input)?;
    Ok((input, PngChunk { length, type_: type_.try_into().unwrap(), data: data.to_vec(), crc }))
}

fn ihdr(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = png_chunk(input)?;
    Ok((input, ()))
}

fn iend(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = png_chunk(input)?;
    Ok((input, ()))
}

fn png_data(input: &[u8]) -> IResult<&[u8], Vec<PngChunk>> {
    let (input, chunks) = many0(png_chunk)(input)?;
    Ok((input, chunks))
}

fn many0<I, O, E, F>(f: F) -> impl Fn(I) -> IResult<I, Vec<O>, E>
where
    F: Fn(I) -> IResult<I, O, E>,
    I: Clone,
{
    move |i: I| {
        let mut res = Vec::new();
        let mut input = i.clone();
        loop {
            match f(input.clone()) {
                Ok((i, o)) => {
                    res.push(o);
                    input = i;
                }
                Err(nom::Err::Error(_)) | Err(nom::Err::Failure(_)) => break,
                Err(nom::Err::Incomplete(_)) => {
                    return Err(nom::Err::Incomplete(_));
                }
            }
        }
        Ok((input, res))
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

    match png_header(&buffer) {
        Ok((remaining, header)) => {
            println!("PNG Header: {:?}", header);
            match png_data(remaining) {
                Ok((_, chunks)) => {
                    println!("Chunks: {:?}", chunks);
                }
                Err(e) => println!("Error parsing chunks: {:?}", e),
            }
        }
        Err(e) => println!("Error parsing header: {:?}", e),
    }
}
