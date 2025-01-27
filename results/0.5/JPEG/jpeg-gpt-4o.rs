use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, verify},
    error::ErrorKind,
    multi::{count, many0},
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

// JPEG markers
const SOI: &[u8] = &[0xFF, 0xD8]; // Start of Image
const EOI: &[u8] = &[0xFF, 0xD9]; // End of Image
const SOS: &[u8] = &[0xFF, 0xDA]; // Start of Scan
const APP0: &[u8] = &[0xFF, 0xE0]; // Application Segment 0
const APP1: &[u8] = &[0xFF, 0xE1]; // Application Segment 1
const DQT: &[u8] = &[0xFF, 0xDB]; // Define Quantization Table
const SOF0: &[u8] = &[0xFF, 0xC0]; // Start of Frame (Baseline DCT)
const DHT: &[u8] = &[0xFF, 0xC4]; // Define Huffman Table

#[derive(Debug)]
struct Jpeg {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    SOI,
    EOI,
    APP0(Vec<u8>),
    APP1(Vec<u8>),
    DQT(Vec<u8>),
    SOF0(Vec<u8>),
    SOS(Vec<u8>),
    DHT(Vec<u8>),
    Unknown(u8, Vec<u8>),
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Jpeg> {
    let (input, _) = tag(SOI)(input)?;
    let (input, segments) = many0(parse_segment)(input)?;
    let (input, _) = tag(EOI)(input)?;
    Ok((input, Jpeg { segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = preceded(tag(&[0xFF]), be_u8)(input)?;
    let (input, length) = verify(be_u16, |&len| len >= 2)(input)?;

    let (input, data) = take(length as usize - 2)(input)?;

    let segment = match marker {
        0xE0 => Segment::APP0(data.to_vec()),
        0xE1 => Segment::APP1(data.to_vec()),
        0xDB => Segment::DQT(data.to_vec()),
        0xC0 => Segment::SOF0(data.to_vec()),
        0xDA => Segment::SOS(data.to_vec()),
        0xC4 => Segment::DHT(data.to_vec()),
        _ => Segment::Unknown(marker, data.to_vec()),
    };

    Ok((input, segment))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_jpeg(&buffer) {
        Ok((_, jpeg)) => println!("{:?}", jpeg),
        Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
    }
}