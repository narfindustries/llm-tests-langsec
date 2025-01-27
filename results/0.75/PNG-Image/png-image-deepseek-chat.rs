use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

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
struct PngChunk {
    length: u32,
    chunk_type: [u8; 4],
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct PngFile {
    header: PngHeader,
    chunks: Vec<PngChunk>,
}

fn parse_png_header(input: &[u8]) -> IResult<&[u8], PngHeader> {
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) =
        tuple((be_u32, be_u32, be_u8, be_u8, be_u8, be_u8, be_u8))(input)?;
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

fn parse_png_chunk(input: &[u8]) -> IResult<&[u8], PngChunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;
    Ok((
        input,
        PngChunk {
            length,
            chunk_type: *array_ref!(chunk_type, 0, 4),
            data: data.to_vec(),
            crc,
        },
    ))
}

fn parse_png_file(input: &[u8]) -> IResult<&[u8], PngFile> {
    let (input, _) = tag([0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A])(input)?;
    let (input, header) = parse_png_header(input)?;
    let mut chunks = Vec::new();
    let mut remaining_input = input;
    while !remaining_input.is_empty() {
        let (input, chunk) = parse_png_chunk(remaining_input)?;
        remaining_input = input;
        chunks.push(chunk);
    }
    Ok((remaining_input, PngFile { header, chunks }))
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
    let result = parse_png_file(&buffer);
    match result {
        Ok((_, png_file)) => println!("{:#?}", png_file),
        Err(e) => eprintln!("Failed to parse PNG file: {:?}", e),
    }
    Ok(())
}