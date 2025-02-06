use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    error::ParseError,
    number::complete::be_u32,
    IResult,
};
use std::fs::read;
use std::path::Path;

#[derive(Debug)]
struct PngSignature;

#[derive(Debug)]
struct PngChunk {
    length: u32,
    chunk_type: [u8; 4],
    data: Vec<u8>,
    crc: u32,
}

fn png_signature(input: &[u8]) -> IResult<&[u8], PngSignature> {
    let signature = tag(&[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]);
    map(signature, |_| PngSignature)(input)
}

fn png_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let (input, data) = take(length as usize)(input)?;
    let (input, crc) = be_u32(input)?;

    Ok((
        input,
        PngChunk {
            length,
            chunk_type: chunk_type.try_into().unwrap(),
            data: data.to_vec(),
            crc,
        },
    ))
}

fn png_chunks(input: &[u8]) -> IResult<&[u8], Vec<PngChunk>> {
    many0(png_chunk)(input)
}

fn many0<I, O, E: ParseError<I>, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    F: FnMut(I) -> IResult<I, O, E>,
    I: Clone,
{
    move |mut i| {
        let mut res = Vec::new();
        loop {
            match f(&mut i) {
                Ok((i1, o)) => {
                    res.push(o);
                    i = i1;
                }
                Err(nom::Err::Error(_)) => break,
                Err(e) => return Err(e),
            }
        }
        Ok((i, res))
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let data = read(Path::new(filename))?;

    match png_signature(&data) {
        Ok((remaining, _signature)) => {
            match png_chunks(remaining) {
                Ok((_, chunks)) => {
                    println!("PNG file parsed successfully. Chunks:");
                    for chunk in chunks {
                        println!("{:?}", chunk);
                    }
                }
                Err(e) => {
                    println!("Error parsing PNG chunks: {:?}", e);
                }
            }
        }
        Err(e) => {
            println!("Error parsing PNG signature: {:?}", e);
        }
    }

    Ok(())
}
