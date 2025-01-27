use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    error::Error,
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;
use std::path::Path;

#[derive(Debug)]
struct JpegSegment {
    marker: u16,
    length: Option<u16>,
    data: Vec<u8>,
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], u16> {
    preceded(tag(&[0xFF]), be_u8)(input).and_then(|(next_input, marker_byte)| {
        Ok((next_input, u16::from_be_bytes([0xFF, marker_byte])))
    })
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, marker) = parse_marker(input)?;

    // SOI and EOI segments don't have a length field
    let segments_without_length = [0xFFD8, 0xFFD9];

    if segments_without_length.contains(&marker) {
        Ok((
            input,
            JpegSegment {
                marker,
                length: None,
                data: Vec::new(),
            },
        ))
    } else {
        let (input, length) = be_u16(input)?;

        // Length includes the 2 bytes of its own size, so we need to subtract 2 for actual data length
        let data_length = length.saturating_sub(2) as usize;
        let (input, data) = take(data_length)(input)?;

        Ok((
            input,
            JpegSegment {
                marker,
                length: Some(length),
                data: data.to_vec(),
            },
        ))
    }
}

fn parse_jpeg(mut input: &[u8]) -> IResult<&[u8], Vec<JpegSegment>> {
    let mut segments = Vec::new();

    loop {
        match parse_segment(input) {
            Ok((next_input, segment)) => {
                // Break the loop if End of Image (EOI) is reached
                if segment.marker == 0xFFD9 {
                    segments.push(segment);
                    break;
                }
                input = next_input;
                segments.push(segment);
            }
            Err(err) => return Err(err),
        }
    }

    Ok((input, segments))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let path = &args[1];
    let path = Path::new(path);
    
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

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

    Ok(())
}