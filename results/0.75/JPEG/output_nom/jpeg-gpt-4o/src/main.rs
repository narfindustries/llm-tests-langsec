use nom::{
    bytes::complete::take,
    number::complete::be_u16,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

// JPEG Markers
const SOI: u16 = 0xFFD8;
const EOI: u16 = 0xFFD9;
const APP0: u16 = 0xFFE0;
const DQT: u16 = 0xFFDB;
const SOF0: u16 = 0xFFC0;
const DHT: u16 = 0xFFC4;
const SOS: u16 = 0xFFDA;
const COM: u16 = 0xFFFE;

#[derive(Debug, Clone)]
struct JpegSegment {
    marker: u16,
    data: Vec<u8>,
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, marker) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length - 2)(input)?;
    Ok((input, JpegSegment { marker, data: data.to_vec() }))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Vec<JpegSegment>> {
    let mut segments = Vec::new();
    let (mut input, _) = be_u16(input)?;
    
    while !input.is_empty() {
        let (next_input, segment) = parse_segment(input)?;
        input = next_input;
        segments.push(segment.clone());

        if segment.marker == EOI {
            break;
        }
    }
    Ok((input, segments))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <JPEG file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_jpeg(&buffer) {
        Ok((_, segments)) => {
            for segment in segments {
                println!("{:?}", segment);
            }
        }
        Err(e) => {
            eprintln!("Error parsing JPEG: {:?}", e);
        }
    }
}