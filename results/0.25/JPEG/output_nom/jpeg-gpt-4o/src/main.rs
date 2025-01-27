use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    error::ParseError,
    multi::many_till,
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use std::env;

#[derive(Debug)]
struct JpegSegment {
    marker: u16,
    length: Option<u16>,
    data: Vec<u8>,
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], u16> {
    preceded(tag(&[0xFF]), be_u8)(input).map(|(next_input, marker)| (next_input, 0xFF00 | marker as u16))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, marker) = parse_marker(input)?;
    let (input, length) = if marker != 0xFFD8 && marker != 0xFFD9 {
        let (input, length) = be_u16(input)?;
        let (input, data) = take(length - 2)(input)?;
        (input, Some((length, data.to_vec())))
    } else {
        (input, None)
    };
    let data = length.map_or_else(Vec::new, |(_, data)| data);
    Ok((input, JpegSegment { marker, length: length.map(|(l, _)| l), data }))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Vec<JpegSegment>> {
    many_till(parse_segment, verify(parse_marker, |&m| m == 0xFFD9))(input).map(|(next_input, (segments, _))| (next_input, segments))
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input_file = &args[1];
    match read_file(input_file) {
        Ok(data) => match parse_jpeg(&data) {
            Ok((_, segments)) => {
                for segment in segments {
                    println!("{:?}", segment);
                }
            }
            Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
        },
        Err(e) => eprintln!("Failed to read file: {:?}", e),
    }
}