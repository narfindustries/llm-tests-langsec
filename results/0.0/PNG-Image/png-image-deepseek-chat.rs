use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::length_count,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

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

fn parse_png(input: &[u8]) -> IResult<&[u8], PNG> {
    let (input, signature) = parse_signature(input)?;
    let (input, chunks) = parse_chunks(input)?;
    Ok((input, PNG { signature, chunks }))
}

fn parse_signature(input: &[u8]) -> IResult<&[u8], [u8; 8]> {
    let (input, signature) = take(8usize)(input)?;
    let signature_array: [u8; 8] = signature.try_into().unwrap();
    Ok((input, signature_array))
}

fn parse_chunks(input: &[u8]) -> IResult<&[u8], Vec<Chunk>> {
    length_count(parse_chunk, input)
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, (length, chunk_type, data, crc)) = tuple((
        be_u32,
        take(4usize),
        map_res(be_u32, |len| take(len as usize)),
        be_u32,
    ))(input)?;

    let chunk_type_array: [u8; 4] = chunk_type.try_into().unwrap();
    let data_vec: Vec<u8> = data.to_vec();

    Ok((
        input,
        Chunk {
            length,
            chunk_type: chunk_type_array,
            data: data_vec,
            crc,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_png(&buffer) {
        Ok((_, png)) => println!("{:#?}", png),
        Err(e) => eprintln!("Failed to parse PNG: {:?}", e),
    }

    Ok(())
}