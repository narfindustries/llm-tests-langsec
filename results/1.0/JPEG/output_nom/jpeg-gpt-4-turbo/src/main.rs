use nom::{
    bytes::complete::{tag, take, take_until},
    combinator::map,
    multi::many0,
    number::complete::{be_u16, be_u8},
    IResult,
};

use std::fs::File;
use std::io::{Read};
use std::env;

// JPEG Markers
const SOI_MARKER: &[u8] = &[0xFF, 0xD8]; // Start Of Image
const EOI_MARKER: &[u8] = &[0xFF, 0xD9]; // End Of Image
const SOS_MARKER: &[u8] = &[0xFF, 0xDA]; // Start Of Scan
const DQT_MARKER: &[u8] = &[0xFF, 0xDB]; // Define Quantization Tables
const SOF0_MARKER: &[u8] = &[0xFF, 0xC0]; // Start of Frame (Baseline DCT)
const DHT_MARKER: &[u8] = &[0xFF, 0xC4]; // Define Huffman Tables
const APP0_MARKER: &[u8] = &[0xFF, 0xE0]; // Application Segment 0 (JFIF)
const APP1_MARKER: &[u8] = &[0xFF, 0xE1]; // Application Segment 1 (Exif)
const COM_MARKER: &[u8] = &[0xFF, 0xFE]; // Comment

/// JPEG segment parsers
fn parse_marker(input: &[u8]) -> IResult<&[u8], &[u8]> {
    tag(&[0xFF])(input)
}

fn parse_soi(input: &[u8]) -> IResult<&[u8], ()> {
    map(tag(SOI_MARKER), |_| ())(input)
}

fn parse_eoi(input: &[u8]) -> IResult<&[u8], ()> {
    map(tag(EOI_MARKER), |_| ())(input)
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], (u16, u16, u8)> {
    let (input, _) = tag(SOF0_MARKER)(input)?;
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;

    assert!(length == 8 + 3 * num_components as u16);

    // Skipping components details
    let (input, _) = take(3 * num_components)(input)?;

    Ok((input, (width, height, precision)))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = tag(DQT_MARKER)(input)?;
    let (input, length) = be_u16(input)?;

    // Skipping the actual quantization table details
    let (input, tables) = take(length - 2)(input)?;

    Ok((input, tables.to_vec()))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = tag(DHT_MARKER)(input)?;
    let (input, length) = be_u16(input)?;

    // Skipping the actual Huffman table details
    let (input, tables) = take(length - 2)(input)?;

    Ok((input, tables.to_vec()))
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = tag(APP0_MARKER)(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length - 2)(input)?;

    Ok((input, data.to_vec()))
}

fn parse_app1(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = tag(APP1_MARKER)(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length - 2)(input)?;

    Ok((input, data.to_vec()))
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = parse_sos(input)?;
    let (input, image_data) = take_until(EOI_MARKER)(input)?;

    Ok((input, image_data.to_vec())) 
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], ()> {
    map(tag(SOS_MARKER), |_| ())(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} <JPEG_FILE>", args[0]);
        return;
    }

    let mut file = match File::open(&args[1]) {
        Ok(file) => file,
        Err(e) => {
            println!("Failed to open file '{}': {}", args[1], e);
            return;
        }
    };

    let mut data = Vec::new();
    if let Err(e) = file.read_to_end(&mut data) {
        println!("Failed to read file '{}': {}", args[1], e);
        return;
    }

    match parse_image_data(&data) {
        Ok((_, image_data)) => {
            println!("Successfully parsed JPEG file. Image data size: {} bytes.", image_data.len());
        },
        Err(e) => {
            println!("Failed to parse JPEG file: {:?}", e);
        }
    }
}