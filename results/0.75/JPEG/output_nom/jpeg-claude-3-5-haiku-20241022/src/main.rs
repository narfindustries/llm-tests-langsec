use nom::{
    bytes::complete::{tag, take},
    multi::many0,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct JpegSegment {
    marker: u16,
    length: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct JpegFile {
    segments: Vec<JpegSegment>,
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], u16> {
    be_u16(input)
}

fn parse_length(input: &[u8]) -> IResult<&[u8], u16> {
    be_u16(input)
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, marker) = parse_marker(input)?;
    let (input, length) = parse_length(input)?;
    let (input, data) = take(length - 2)(input)?;
    
    Ok((input, JpegSegment {
        marker,
        length,
        data: data.to_vec(),
    }))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegFile> {
    let (input, _) = tag(&[0xFF, 0xD8])(input)?;
    let (input, segments) = many0(parse_segment)(input)?;
    let (input, _) = tag(&[0xFF, 0xD9])(input)?;
    
    Ok((input, JpegFile { segments }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <jpeg_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_jpeg(&buffer) {
        Ok((_, jpeg)) => {
            println!("Parsed JPEG with {} segments", jpeg.segments.len());
            for segment in jpeg.segments {
                println!("Marker: 0x{:04X}, Length: {}", segment.marker, segment.length);
            }
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse JPEG: {:?}", e);
            std::process::exit(1);
        }
    }
}