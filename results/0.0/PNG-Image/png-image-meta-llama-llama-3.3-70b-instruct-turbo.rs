use nom::{
    bytes::complete::{tag},
    multi::{length_data},
    number::complete::{be_u32, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};
use std::path::Path;

#[derive(Debug, PartialEq)]
enum ColorType {
    Grayscale,
    RGB,
    Plte,
    GreyscaleAlpha,
    RGBA,
}

#[derive(Debug, PartialEq)]
enum CompressionMethod {
    DeflateInflate,
}

#[derive(Debug, PartialEq)]
enum FilterMethod {
    Adaptive,
}

#[derive(Debug, PartialEq)]
enum InterlaceMethod {
    NoInterlace,
    Adam7Interlace,
}

#[derive(Debug, PartialEq)]
struct IHDR {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: ColorType,
    compression_method: CompressionMethod,
    filter_method: FilterMethod,
    interlace_method: InterlaceMethod,
}

fn parse_magic_number(input: &[u8]) -> IResult<&[u8], ()> {
    static MAGIC_NUMBER: [u8; 8] = [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a];
    tag(&MAGIC_NUMBER)(input).map(|(input, _)| (input, ()))
}

fn parse_ihdr(input: &[u8]) -> IResult<&[u8], IHDR> {
    let (input, _) = tag([0x49, 0x48, 0x44, 0x52])(input)?;
    let (input, width) = be_u32(input)?;
    let (input, height) = be_u32(input)?;
    let (input, bit_depth) = be_u8(input)?;
    let (input, color_type) = be_u8(input)?;
    let (input, compression_method) = be_u8(input)?;
    let (input, filter_method) = be_u8(input)?;
    let (input, interlace_method) = be_u8(input)?;
    Ok((
        input,
        IHDR {
            width,
            height,
            bit_depth,
            color_type: match color_type {
                0 => ColorType::Grayscale,
                2 => ColorType::RGB,
                3 => ColorType::Plte,
                4 => ColorType::GreyscaleAlpha,
                6 => ColorType::RGBA,
                _ => panic!("Invalid color type"),
            },
            compression_method: match compression_method {
                0 => CompressionMethod::DeflateInflate,
                _ => panic!("Invalid compression method"),
            },
            filter_method: match filter_method {
                0 => FilterMethod::Adaptive,
                _ => panic!("Invalid filter method"),
            },
            interlace_method: match interlace_method {
                0 => InterlaceMethod::NoInterlace,
                1 => InterlaceMethod::Adam7Interlace,
                _ => panic!("Invalid interlace method"),
            },
        },
    ))
}

fn parse_plte(input: &[u8]) -> IResult<&[u8], Vec<(u8, u8, u8)>> {
    let (input, _) = tag([0x50, 0x4c, 0x54, 0x45])(input)?;
    let (input, palette_data) = length_data(be_u32)(input)?;
    let palette_entries: Vec<(u8, u8, u8)> = palette_data
        .chunks(3)
        .map(|chunk| (chunk[0], chunk[1], chunk[2]))
        .collect();
    Ok((input, palette_entries))
}

fn parse_idat(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = tag([0x49, 0x44, 0x41, 0x54])(input)?;
    let (input, idat_data) = length_data(be_u32)(input)?;
    Ok((input, idat_data.to_vec()))
}

fn parse_iend(input: &[u8]) -> IResult<&[u8], ()> {
    tag([0x49, 0x45, 0x4e, 0x44])(input).map(|(input, _)| (input, ()))
}

fn parse_png(input: &[u8]) -> IResult<&[u8], (IHDR, Vec<(u8, u8, u8)>, Vec<u8>)> {
    let (input, _) = parse_magic_number(input)?;
    let (input, ihdr) = parse_ihdr(input)?;
    let (input, plte) = parse_plte(input)?;
    let (input, idat) = parse_idat(input)?;
    let (input, _) = parse_iend(input)?;
    Ok((input, (ihdr, plte, idat)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let path = Path::new(input_file);
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    match parse_png(&input) {
        Ok((remaining, (ihdr, plte, idat))) => {
            println!("IHDR: {:?}", ihdr);
            println!("PLTE: {:?}", plte);
            println!("IDAT: {:?}", idat);
            println!("Remaining: {:?}", remaining);
        }
        Err(err) => {
            println!("Error: {:?}", err);
        }
    }
}