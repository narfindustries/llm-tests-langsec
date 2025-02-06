use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    error::ErrorKind,
    multi::many0,
    number::complete::be_u16,
    sequence::preceded,
    IResult,
};
use std::fs::read;
use std::path::Path;
use std::str;

#[derive(Debug)]
enum JpegMarker {
    SOI,
    EOI,
    APP0(Vec<u8>),
    APP1(Vec<u8>),
    DQT(Vec<u8>),
    DHT(Vec<u8>),
    DRI(u16),
    SOS(Vec<u8>),
    COM(String),
    Unknown(u8, Vec<u8>),
}

fn jpeg_marker(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    let (input, marker) = preceded(tag(b"\xFF"), take(1usize))(input)?;
    match marker[0] {
        0xd8 => Ok((input, JpegMarker::SOI)),
        0xd9 => Ok((input, JpegMarker::EOI)),
        0xe0 => {
            let (input, data) = read_jpeg_segment(input)?;
            Ok((input, JpegMarker::APP0(data)))
        }
        0xe1 => {
            let (input, data) = read_jpeg_segment(input)?;
            Ok((input, JpegMarker::APP1(data)))
        }
        0xdb => {
            let (input, data) = read_jpeg_segment(input)?;
            Ok((input, JpegMarker::DQT(data)))
        }
        0xc4 => {
            let (input, data) = read_jpeg_segment(input)?;
            Ok((input, JpegMarker::DHT(data)))
        }
        0xdd => {
            let (input, restart_interval) = preceded(take(2usize), be_u16)(input)?;
            Ok((input, JpegMarker::DRI(restart_interval)))
        }
        0xda => {
            let (input, data) = read_jpeg_segment(input)?;
            Ok((input, JpegMarker::SOS(data)))
        }
        0xfe => {
            let (input, data) = read_jpeg_segment(input)?;
            match str::from_utf8(&data) {
                Ok(comment) => Ok((input, JpegMarker::COM(comment.to_string()))),
                Err(_) => Ok((input, JpegMarker::Unknown(marker[0], data))),
            }
        }
        _ => {
            let (input, data) = read_jpeg_segment(input)?;
            Ok((input, JpegMarker::Unknown(marker[0], data)))
        }
    }
}

fn read_jpeg_segment(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = preceded(take(2usize), be_u16)(input)?;
    let (input, data) = take((length - 2) as usize)(input)?;
    Ok((input, data.to_vec()))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Vec<JpegMarker>> {
    many0(jpeg_marker)(input)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <jpeg_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let data = match read(path) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            std::process::exit(1);
        }
    };

    match parse_jpeg(&data) {
        Ok((_, markers)) => {
            println!("{:#?}", markers);
        }
        Err(e) => {
            eprintln!("Error parsing JPEG: {:?}", e);
            std::process::exit(1);
        }
    }
}
