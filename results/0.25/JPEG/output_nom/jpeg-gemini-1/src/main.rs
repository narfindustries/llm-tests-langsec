use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    number::complete::{be_u16, be_u8},
    sequence::preceded,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone)]
enum JpegMarker {
    SOI,
    EOI,
    APPn(u8, Vec<u8>),
    DQT(Vec<u8>),
    DHT(Vec<u8>),
    DRI(u16),
    SOS(Vec<u8>),
    Unknown(u8, Vec<u8>),
}

fn jpeg_marker(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    let (input, marker) = preceded(tag(b"\xFF"), be_u8)(input)?;

    match marker {
        0xd8 => Ok((input, JpegMarker::SOI)),
        0xd9 => Ok((input, JpegMarker::EOI)),
        0xe0..=0xef => {
            let (input, len) = preceded(tag(b"\xFF"), be_u16)(input)?;
            let (input, data) = take(len as usize - 2)(input)?;
            Ok((input, JpegMarker::APPn(marker - 0xe0, data.to_vec())))
        }
        0xdb => {
            let (input, len) = preceded(tag(b"\xFF"), be_u16)(input)?;
            let (input, data) = take(len as usize - 2)(input)?;
            Ok((input, JpegMarker::DQT(data.to_vec())))
        }
        0xc4 => {
            let (input, len) = preceded(tag(b"\xFF"), be_u16)(input)?;
            let (input, data) = take(len as usize - 2)(input)?;
            Ok((input, JpegMarker::DHT(data.to_vec())))
        }
        0xdd => {
            let (input, len) = preceded(tag(b"\xFF"), be_u16)(input)?;
            let (input, restart_interval) = be_u16(input)?;
            Ok((input, JpegMarker::DRI(restart_interval)))
        }
        0xda => {
            let (input, len) = preceded(tag(b"\xFF"), be_u16)(input)?;
            let (input, data) = take(len as usize - 2)(input)?;
            Ok((input, JpegMarker::SOS(data.to_vec())))
        }
        _ => {
            let (input, len) = preceded(tag(b"\xFF"), be_u16)(input)?;
            let (input, data) = take(len as usize - 2)(input)?;
            Ok((input, JpegMarker::Unknown(marker, data.to_vec())))
        }
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
        println!("Usage: cargo run <jpeg_file>");
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_jpeg(&buffer) {
        Ok((_, markers)) => {
            println!("Parsed JPEG markers: {:?}", markers);
        }
        Err(e) => {
            println!("Error parsing JPEG: {:?}", e);
        }
    }
}
