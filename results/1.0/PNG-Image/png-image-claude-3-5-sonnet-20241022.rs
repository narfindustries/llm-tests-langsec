use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::many0,
    number::complete::{be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs, path::Path};

const PNG_SIGNATURE: [u8; 8] = [137, 80, 78, 71, 13, 10, 26, 10];

#[derive(Debug)]
struct PngImage {
    ihdr: IHDR,
    chunks: Vec<Chunk>,
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
    chunk_type: [u8; 4],
    chunk_data: Vec<u8>,
    crc: u32,
}

fn parse_png_signature(input: &[u8]) -> IResult<&[u8], &[u8]> {
    tag(PNG_SIGNATURE)(input)
}

fn parse_ihdr(input: &[u8]) -> IResult<&[u8], IHDR> {
    let (input, (length, chunk_type, chunk_data, crc)) = parse_chunk(input)?;
    if chunk_type != *b"IHDR" || length != 13 {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }

    let (_, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) =
        tuple((
            be_u32,
            be_u32,
            be_u8,
            be_u8,
            be_u8,
            be_u8,
            be_u8,
        ))(&chunk_data)?;

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

fn parse_chunk(input: &[u8]) -> IResult<&[u8], (u32, [u8; 4], Vec<u8>, u32)> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = map(take(4usize), |b: &[u8]| {
        let mut arr = [0u8; 4];
        arr.copy_from_slice(b);
        arr
    })(input)?;
    let (input, chunk_data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;

    Ok((
        input,
        (length, chunk_type, chunk_data.to_vec(), crc),
    ))
}

fn parse_chunks(input: &[u8]) -> IResult<&[u8], Vec<Chunk>> {
    many0(map(parse_chunk, |(length, chunk_type, chunk_data, crc)| Chunk {
        length,
        chunk_type,
        chunk_data,
        crc,
    }))(input)
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, _) = parse_png_signature(input)?;
    let (input, ihdr) = parse_ihdr(input)?;
    let (input, chunks) = parse_chunks(input)?;

    Ok((input, PngImage { ihdr, chunks }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        std::process::exit(1);
    }

    let file_path = Path::new(&args[1]);
    let data = fs::read(file_path)?;

    match parse_png(&data) {
        Ok((_, png)) => {
            println!("Successfully parsed PNG:");
            println!("IHDR:");
            println!("  Width: {}", png.ihdr.width);
            println!("  Height: {}", png.ihdr.height);
            println!("  Bit Depth: {}", png.ihdr.bit_depth);
            println!("  Color Type: {}", png.ihdr.color_type);
            println!("  Compression Method: {}", png.ihdr.compression_method);
            println!("  Filter Method: {}", png.ihdr.filter_method);
            println!("  Interlace Method: {}", png.ihdr.interlace_method);
            println!("\nChunks:");
            for chunk in png.chunks {
                println!(
                    "  {} ({} bytes)",
                    String::from_utf8_lossy(&chunk.chunk_type),
                    chunk.length
                );
            }
        }
        Err(e) => eprintln!("Failed to parse PNG: {:?}", e),
    }

    Ok(())
}