use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    error::ErrorKind,
    multi::many0,
    number::complete::{be_u16, be_u8},
    sequence::{pair, preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::env;

#[derive(Debug)]
struct JpegSegment {
    marker: u16,
    length: Option<u16>,
    data: Vec<u8>,
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], u16> {
    preceded(tag(&[0xFF]), be_u8)(input).and_then(|(next_input, marker)| {
        if marker == 0x00 || marker == 0xFF {
            Err(nom::Err::Error((next_input, ErrorKind::Tag)))
        } else {
            Ok((next_input, 0xFF00 | marker as u16))
        }
    })
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, marker) = parse_marker(input)?;
    if marker == 0xFFD8 || marker == 0xFFD9 {
        // SOI or EOI markers have no length or data
        Ok((input, JpegSegment { marker, length: None, data: vec![] }))
    } else {
        let (input, length) = be_u16(input)?;
        let (input, data) = take(length - 2)(input)?;
        Ok((input, JpegSegment { marker, length: Some(length), data: data.to_vec() }))
    }
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Vec<JpegSegment>> {
    many0(parse_segment)(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input_path = Path::new(&args[1]);
    let mut file = File::open(&input_path).expect("Failed to open input file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read input file");

    match parse_jpeg(&buffer) {
        Ok((_, segments)) => {
            for segment in segments {
                println!("{:?}", segment);
            }
        }
        Err(e) => {
            eprintln!("Failed to parse JPEG: {:?}", e);
            std::process::exit(1);
        }
    }
}