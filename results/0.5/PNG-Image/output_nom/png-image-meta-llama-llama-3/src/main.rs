use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    error::ErrorKind,
    multi::{length_data, many_till},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{BufReader, Read};
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
    Deflate,
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
enum Unit {
    Unknown,
    Meter,
}

#[derive(Debug, PartialEq)]
struct Ihdr {
    width: u32,
    height: u32,
    bit_depth: u8,
    color_type: ColorType,
    compression_method: CompressionMethod,
    filter_method: FilterMethod,
    interlace_method: InterlaceMethod,
}

impl Ihdr {
    fn parse(input: &[u8]) -> IResult<&[u8], Ihdr> {
        map(
            tuple((
                be_u32,
                be_u32,
                be_u8,
                map(be_u8, |color_type| match color_type {
                    0 => ColorType::Grayscale,
                    2 => ColorType::RGB,
                    3 => ColorType::Plte,
                    4 => ColorType::GreyscaleAlpha,
                    6 => ColorType::RGBA,
                    _ => panic!("Invalid color type"),
                }),
                map(be_u8, |_| CompressionMethod::Deflate),
                map(be_u8, |_| FilterMethod::Adaptive),
                map(be_u8, |interlace_method| match interlace_method {
                    0 => InterlaceMethod::NoInterlace,
                    1 => InterlaceMethod::Adam7Interlace,
                    _ => panic!("Invalid interlace method"),
                }),
            )),
            |(width, height, bit_depth, color_type, compression_method, filter_method, interlace_method)| Ihdr {
                width,
                height,
                bit_depth,
                color_type,
                compression_method,
                filter_method,
                interlace_method,
            },
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct Plte {
    palette_entries: Vec<(u8, u8, u8)>,
}

impl Plte {
    fn parse(input: &[u8]) -> IResult<&[u8], Plte> {
        map(
            many_till(
                tuple((be_u8, be_u8, be_u8)),
                tag(&[0; 0]),
            ),
            |(palette_entries, _)| Plte { palette_entries },
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct Idat {
    compressed_image_data: Vec<u8>,
}

impl Idat {
    fn parse(input: &[u8]) -> IResult<&[u8], Idat> {
        map(take_while(|c| c != 0), |compressed_image_data| {
            Idat {
                compressed_image_data: compressed_image_data.to_vec(),
            }
        })(input)
    }
}

fn take_while<F>(predicate: F) -> impl FnMut(&[u8]) -> IResult<&[u8], &[u8]>
where
    F: Fn(u8) -> bool + Clone,
{
    move |input: &[u8]| {
        let mut i = 0;
        while i < input.len() && predicate(input[i]) {
            i += 1;
        }
        Ok((&input[i..], &input[..i]))
    }
}

#[derive(Debug, PartialEq)]
struct Png {
    ihdr: Ihdr,
    plte: Option<Plte>,
    idat: Idat,
    iend: (),
}

impl Png {
    fn parse(input: &[u8]) -> IResult<&[u8], Png> {
        map(
            tuple((
                preceded(tag(&[0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]), Ihdr::parse),
                opt(Plte::parse),
                Idat::parse,
                tag(&[0x49, 0x45, 0x4e, 0x44, 0xae, 0x42, 0x60, 0x82]),
            )),
            |(ihdr, plte, idat, _)| Png { ihdr, plte, idat, iend: () },
        )(input)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let path = Path::new(input_file);
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    match Png::parse(&input) {
        Ok((_, png)) => println!("{:?}", png),
        Err(err) => eprintln!("Error parsing PNG: {:?}", err),
    }
}