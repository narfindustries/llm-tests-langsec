use nom::bytes::complete::{tag, take};
use nom::combinator::{map, map_res};
use nom::multi::count;
use nom::number::complete::be_u16;
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct JpegSegment {
    marker: u16,
    length: Option<u16>,
    data: Option<Vec<u8>>,
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], u16> {
    map(be_u16, |marker| {
        if marker == 0xFFD8 || marker == 0xFFD9 {
            marker
        } else {
            marker & 0xFFFF
        }
    })(input)
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, marker) = parse_marker(input)?;
    if marker == 0xFFD8 || marker == 0xFFD9 {
        Ok((input, JpegSegment { marker, length: None, data: None }))
    } else {
        let (input, length) = be_u16(input)?;
        let (input, data) = take(length - 2)(input)?;
        Ok((input, JpegSegment { marker, length: Some(length), data: Some(data.to_vec()) }))
    }
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Vec<JpegSegment>> {
    let mut input = input;
    let mut segments = Vec::new();

    while !input.is_empty() {
        let (i, segment) = parse_segment(input)?;
        segments.push(segment);
        input = i;
        if let Some(JpegSegment { marker: 0xFFD9, .. }) = segments.last() {
            break;
        }
    }

    Ok((input, segments))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <jpeg_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Failed to open file: {}", e);
            return;
        }
    };

    let mut buffer = Vec::new();
    if let Err(e) = file.read_to_end(&mut buffer) {
        eprintln!("Failed to read file: {}", e);
        return;
    }

    match parse_jpeg(&buffer) {
        Ok((_, segments)) => {
            for segment in segments {
                println!("{:?}", segment);
            }
        }
        Err(e) => {
            eprintln!("Failed to parse JPEG: {:?}", e);
        }
    }
}