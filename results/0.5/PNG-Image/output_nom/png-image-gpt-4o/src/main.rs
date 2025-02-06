use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::many0,
    number::complete::{be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug)]
struct PNG {
    signature: [u8; 8],
    chunks: Vec<Chunk>,
}

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: [u8; 4],
    data: Vec<u8>,
    crc: u32,
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

fn parse_png(input: &[u8]) -> IResult<&[u8], PNG> {
    let (input, signature) = parse_signature(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;
    Ok((input, PNG { signature, chunks }))
}

fn parse_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    map(tag(&[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]), |s: &[u8]| {
        let mut sig = [0; 8];
        sig.copy_from_slice(s);
        sig
    })(input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = map(take(4usize), |s: &[u8]| {
        let mut ct = [0; 4];
        ct.copy_from_slice(s);
        ct
    })(input)?;
    let (input, data) = take(length as usize)(input)?;
    let (input, crc) = be_u32(input)?;
    Ok((
        input,
        Chunk {
            length,
            chunk_type,
            data: data.to_vec(),
            crc,
        },
    ))
}

fn parse_ihdr(input: &[u8]) -> IResult<&[u8], IHDR> {
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) =
        tuple((be_u32, be_u32, be_u8, be_u8, be_u8, be_u8, be_u8))(input)?;
    Ok((
        input,
        IHDR {
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

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    match parse_png(&buffer) {
        Ok((_, png)) => {
            println!("{:?}", png);
            for chunk in &png.chunks {
                if &chunk.chunk_type == b"IHDR" {
                    match parse_ihdr(&chunk.data) {
                        Ok((_, ihdr)) => println!("{:?}", ihdr),
                        Err(e) => eprintln!("Error parsing IHDR: {:?}", e),
                    }
                }
            }
        }
        Err(e) => eprintln!("Error parsing PNG: {:?}", e),
    }
}