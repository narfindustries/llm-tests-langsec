use nom::{
    bytes::complete::{tag, take},
    multi::{count, many0},
    number::complete::{be_u8, be_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct Png {
    signature: [u8; 8],
    chunks: Vec<Chunk>,
}

#[derive(Debug)]
enum Chunk {
    IHDR(IHDRData),
    PLTE(Vec<RGB>),
    IDAT(Vec<u8>),
    IEND,
    Other(String, Vec<u8>),
}

#[derive(Debug)]
struct IHDRData {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: u8,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

#[derive(Debug)]
struct RGB {
    red: u8,
    green: u8,
    blue: u8,
}

fn parse_png(input: &[u8]) -> IResult<&[u8], Png> {
    let (input, signature) = tag(b"\x89PNG\r\n\x1a\n")(input)?;
    let (input, chunks) = many0(parse_chunk)(input)?;

    let signature_array = <[u8; 8]>::try_from(signature).expect("Slice with incorrect length");

    Ok((input, Png { signature: signature_array, chunks }))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, (length, chunk_type)) = tuple((be_u32, take(4usize)))(input)?;
    let (input, data) = take(length)(input)?;
    let (input, _) = take(4usize)(input)?;

    match chunk_type {
        b"IHDR" => {
            let (_, ihdr_data) = parse_ihdr(data)?;
            Ok((input, Chunk::IHDR(ihdr_data)))
        },
        b"PLTE" => {
            let (_, plte_data) = count(parse_rgb, (length / 3) as usize)(data)?;
            Ok((input, Chunk::PLTE(plte_data)))
        },
        b"IDAT" => {
            Ok((input, Chunk::IDAT(data.to_vec())))
        },
        b"IEND" => {
            Ok((input, Chunk::IEND))
        },
        _ => {
            let chunk_type_string = String::from_utf8(chunk_type.to_vec()).unwrap();
            Ok((input, Chunk::Other(chunk_type_string, data.to_vec())))
        }
    }
}

fn parse_ihdr(input: &[u8]) -> IResult<&[u8], IHDRData> {
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) =
        tuple((be_u32, be_u32, be_u8, be_u8, be_u8, be_u8, be_u8))(input)?;

    Ok((
        input,
        IHDRData {
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

fn parse_rgb(input: &[u8]) -> IResult<&[u8], RGB> {
    let (input, (red, green, blue)) = tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((input, RGB { red, green, blue }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_png(&buffer) {
        Ok((_, png)) => println!("{:#?}", png),
        Err(e) => eprintln!("Error parsing PNG: {:?}", e),
    }

    Ok(())
}