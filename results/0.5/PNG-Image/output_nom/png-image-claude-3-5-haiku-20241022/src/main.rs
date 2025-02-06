use nom::{
    bytes::complete::{tag, take},
    combinator::{map},
    multi::{count, many0},
    number::complete::{be_u32, be_u8},
    sequence::{tuple, preceded},
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
    Truecolor,
    IndexedColor,
    GrayscaleAlpha,
    TruecolorAlpha,
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
    red: u8,
    green: u8,
    blue: u8,
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
    transparency: Option<Vec<u8>>,
    background: Option<Vec<u8>>,
    gamma: Option<u32>,
    chromaticity: Option<Vec<u8>>,
    physical_dimensions: Option<(u32, u32, u8)>,
    time: Option<(u16, u8, u8, u8, u8, u8)>,
    text_chunks: Vec<(String, String)>,
    idat_chunks: Vec<Vec<u8>>,
}

fn parse_signature(input: &[u8]) -> IResult<&[u8], PngSignature> {
    let (input, _) = tag(&[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A])(input)?;
    Ok((input, PngSignature))
}

fn parse_color_type(color_type: u8) -> ColorType {
    match color_type {
        0 => ColorType::Grayscale,
        2 => ColorType::Truecolor,
        3 => ColorType::IndexedColor,
        4 => ColorType::GrayscaleAlpha,
        6 => ColorType::TruecolorAlpha,
        _ => panic!("Invalid color type"),
    }
}

fn parse_image_header(input: &[u8]) -> IResult<&[u8], ImageHeader> {
    let (input, (width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)) = 
        tuple((
            be_u32,
            be_u32,
            be_u8,
            be_u8,
            be_u8,
            be_u8,
            be_u8
        ))(input)?;

    Ok((input, ImageHeader {
        width,
        height,
        bit_depth,
        color_type: parse_color_type(color_type),
        compression_method,
        filter_method,
        interlace_method,
    }))
}

fn parse_chunk(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, length) = be_u32(input)?;
    let (input, chunk_type) = map(take(4usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string())(input)?;
    let (input, data) = take(length)(input)?;
    let (input, crc) = be_u32(input)?;

    Ok((input, Chunk {
        length,
        chunk_type,
        data: data.to_vec(),
        crc,
    }))
}

fn parse_palette(input: &[u8]) -> IResult<&[u8], Vec<PaletteEntry>> {
    let palette_entry = map(
        tuple((be_u8, be_u8, be_u8)),
        |(red, green, blue)| PaletteEntry { red, green, blue }
    );
    count(palette_entry, input.len() / 3)(input)
}

fn parse_png(input: &[u8]) -> IResult<&[u8], PngImage> {
    let (input, signature) = parse_signature(input)?;
    let (input, ihdr_chunk) = preceded(
        tag(b"IHDR"),
        parse_image_header
    )(input)?;

    let mut png_image = PngImage {
        signature,
        ihdr: ihdr_chunk,
        palette: None,
        transparency: None,
        background: None,
        gamma: None,
        chromaticity: None,
        physical_dimensions: None,
        time: None,
        text_chunks: Vec::new(),
        idat_chunks: Vec::new(),
    };

    let (mut input, mut chunks) = many0(parse_chunk)(input)?;

    for chunk in chunks {
        match chunk.chunk_type.as_str() {
            "PLTE" => png_image.palette = Some(parse_palette(&chunk.data).unwrap().1),
            "tRNS" => png_image.transparency = Some(chunk.data),
            "bKGD" => png_image.background = Some(chunk.data),
            "gAMA" => {
                let gamma_value = u32::from_be_bytes(chunk.data.try_into().unwrap());
                png_image.gamma = Some(gamma_value);
            },
            "cHRM" => png_image.chromaticity = Some(chunk.data),
            "pHYs" => {
                let x = u32::from_be_bytes(chunk.data[0..4].try_into().unwrap());
                let y = u32::from_be_bytes(chunk.data[4..8].try_into().unwrap());
                let unit = chunk.data[8];
                png_image.physical_dimensions = Some((x, y, unit));
            },
            "tIME" => {
                let year = u16::from_be_bytes(chunk.data[0..2].try_into().unwrap());
                let month = chunk.data[2];
                let day = chunk.data[3];
                let hour = chunk.data[4];
                let minute = chunk.data[5];
                let second = chunk.data[6];
                png_image.time = Some((year, month, day, hour, minute, second));
            },
            "tEXt" | "zTXt" | "iTXt" => {
                let keyword = chunk.data.split(|&x| x == 0).next().unwrap();
                let text = &chunk.data[keyword.len() + 1..];
                png_image.text_chunks.push((
                    String::from_utf8_lossy(keyword).to_string(),
                    String::from_utf8_lossy(text).to_string()
                ));
            },
            "IDAT" => png_image.idat_chunks.push(chunk.data),
            "IEND" => break,
            _ => {}
        }
    }

    Ok((input, png_image))
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