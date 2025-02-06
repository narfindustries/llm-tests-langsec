use std::env;
use std::fs;
use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    number::complete::{be_u16, be_u8},
    IResult,
};

#[derive(Debug, Clone)]
enum JpegMarker {
    SOI,
    EOI,
    APPn(u8, Vec<u8>),
    DQT(Vec<u8>),
    DHT(Vec<u8>),
    DRI(u16),
    SOS(Vec<u8>),
    COM(String),
    RST(u8),
}

fn jpeg_marker(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    let (input, _) = tag(b"\xFF")(input)?;
    let (input, marker_code) = be_u8(input)?;

    match marker_code {
        0xd8 => Ok((input, JpegMarker::SOI)),
        0xd9 => Ok((input, JpegMarker::EOI)),
        0xda => Ok((input, JpegMarker::SOS(vec![]))),
        0xdb => Ok((input, JpegMarker::DQT(vec![]))),
        0xc4 => Ok((input, JpegMarker::DHT(vec![]))),
        0xdd => {
            let (input, len) = be_u16(input)?;
            let (input, data) = take(len as usize - 2)(input)?;
            let restart_interval = map(be_u16, |x| x)(data)?; //Corrected this line
            Ok((input, JpegMarker::DRI(restart_interval)))
        },
        0xfe => {
            let (input, len) = be_u16(input)?;
            let (input, data) = take(len as usize - 2)(input)?;
            match String::from_utf8(data.to_vec()) {
                Ok(comment) => Ok((input, JpegMarker::COM(comment))),
                Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Fail))),
            }
        },
        0xe0..=0xef => {
            let (input, len) = be_u16(input)?;
            let (input, data) = take(len as usize - 2)(input)?;
            Ok((input, JpegMarker::APPn(marker_code - 0xe0, data.to_vec())))
        }
        0xd0..=0xd7 => Ok((input, JpegMarker::RST(marker_code - 0xd0))),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Vec<JpegMarker>> {
    let mut markers = Vec::new();
    let mut remaining_input = input;
    loop {
        match jpeg_marker(remaining_input) {
            Ok((rest, marker)) => {
                markers.push(marker.clone());
                remaining_input = rest;
                if let JpegMarker::EOI = marker {
                    break;
                }
            }
            Err(e) => {
                println!("Error parsing JPEG: {:?}", e);
                break;
            }
        }
    }
    Ok((remaining_input, markers))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <jpeg_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = match fs::read(filename) {
        Ok(data) => data,
        Err(e) => {
            println!("Error reading file: {}", e);
            return;
        }
    };

    match parse_jpeg(&data) {
        Ok((_, markers)) => {
            println!("Parsed JPEG markers: {:?}", markers);
        }
        Err(e) => {
            println!("Error parsing JPEG: {:?}", e);
        }
    }
}
