use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    error::ParseError,
    multi::{count, many0, many1},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct PngSignature;

#[derive(Debug)]
enum ColorType {
    Grayscale,
    RGB,
    Palette,
    GrayscaleAlpha,
    RGBAlpha,
}

#[derive(Debug)]
struct ImageHeader {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: ColorType,
    compression_method: u8,
    filter_method: u8,
    interlace_method: u8,
}

#[derive(Debug)]
struct PaletteEntry {
    r: u8,
    g: u8,
    b: u8,
}

#[derive(Debug)]
struct Transparency {
    alpha_values: Vec<u8>,
    transparent_color: Option<(u16, u16, u16)>,
}

#[derive(Debug)]
struct Chunk {
    length: u32,
    chunk_type: String,
    data: Vec<u8>,
    crc: u32,
}

#[derive(Debug)]
struct PngImage {
    signature: PngSignature,
    ihdr: ImageHeader,
    palette: Option<Vec<PaletteEntry>>,
    transparency: Option<Transparency>,
    idat: Vec<Chunk>,
    other_chunks: Vec<Chunk>,
}

fn parse_signature(input: &[u8]) -> IResult<&[u8], PngSignature> {
    let (input, _) = tag(&[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A])(input)?;
    Ok((input, PngSignature))
}

fn parse_color_type(color_type: u8) -> ColorType {
    match color_type {
        0 => ColorType::Grayscale,
        2 => ColorType::RGB,
        3 => ColorType::Palette,
        4 => ColorType::GrayscaleAlpha,
        6 => ColorType::RGBAlpha,
        _ => panic!("Invalid color type"),
    }
}

fn parse_ihdr(input: &[u8]) -> IResult<&[u8], ImageHeader> {
    let (input, (length, chunk_type)) = tuple((be_u32, take(4usize)))(input)?;
    
    if length != 13 || chunk_type != b"IHDR" {
        return Err(nom::Err::Error(ParseError::from_error_kind(input, nom::error::ErrorKind::Tag)));
    }

    let (input, (width, height, bit_depth, color_type, compression, filter, interlace)) = tuple((
        be_u32, be_u32, be_u8, be_u8, be_u8, be_u8, be_u8
    ))(input)?;

    let (input, _crc) = be_u32(input)?;

    Ok((input, ImageHeader {
        width,
        height,
        bit_depth,
        color_type: parse_color_type(color_type),
        compression_method: compression,
        filter_method: filter,
        interlace_method: interlace,
    }))
}

fn parse_palette(input: &[u8]) -> IResult<&[u8], Vec<PaletteEntry>> {
    let (input, (length, chunk_type)) = tuple((be_u32, take(4usize)))(input)?;
    
    if chunk_type != b"PLTE" {
        return Err(nom::Err::Error(ParseError::from_error_kind(input, nom::error::ErrorKind::Tag)));
    }

    let palette_count = length / 3;
    let (input, palette) = count(
        map(tuple((be_u8, be_u8, be_u8)), |(r, g, b)| PaletteEntry { r, g, b }),
        palette_count as usize
    )(input)?;

    let (input, _crc) = be_u32(input)?;

    Ok((input, palette))
}

fn parse_transparency(input: &[u8]) -> IResult<&[u8], Transparency> {
    let (input, (length, chunk_type)) = tuple((be_u32, take(4usize)))(input)?;
    
    if chunk_type != b"tRNS" {
        return Err(nom::Err::Error(ParseError::from_error_kind(input, nom::error::ErrorKind::Tag)));
    }

    let (input, transparency) = match length {
        0 => Ok((input, Transparency { alpha_values: vec![], transparent_color: None })),
        _ => {
            let (input, alpha_values) = take(length)(input)?;
            let (input, _crc) = be_u32(input)?;
            Ok((input, Transparency { 
                alpha_values: alpha_values.to_vec(), 
                transparent_color: None 
            }))
        }
    }?;

    Ok((input, transparency))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = take(4usize)(input)?;
    let chunk_type_str = String::from_utf8_lossy(chunk_type).to_string();
    
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;

    Ok((input, Chunk {
        length,
        chunk_type: chunk_type_str,
        data: data.to_vec(),
        crc,
    }))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, signature) = parse_signature(input)?;
    let (input, ihdr) = parse_ihdr(input)?;
    
    let (input, palette) = opt(parse_palette)(input)?;
    let (input, transparency) = opt(parse_transparency)(input)?;

    let (input, idat_chunks) = many1(|i| {
        let (remaining, chunk) = parse_chunk(i)?;
        if chunk.chunk_type == "IDAT" {
            Ok((remaining, chunk))
        } else {
            Err(nom::Err::Error(ParseError::from_error_kind(i, nom::error::ErrorKind::Tag)))
        }
    })(input)?;

    let (input, other_chunks) = many0(parse_chunk)(input)?;

    Ok((input, PngImage {
        signature,
        ihdr,
        palette,
        transparency,
        idat: idat_chunks,
        other_chunks,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <png_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    match parse_png(&buffer) {
        Ok((_, png)) => println!("{:#?}", png),
        Err(e) => eprintln!("Error parsing PNG: {:?}", e),
    }
}