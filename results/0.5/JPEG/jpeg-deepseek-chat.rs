use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16},
    sequence::tuple,
    IResult,
    multi::many0,
    combinator::map_res,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct JPEG {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    SOI,
    EOI,
    APP(u8, Vec<u8>),
    DQT(Vec<u8>),
    SOF0(u16, u8, u16, u16, Vec<Component>),
    DHT(Vec<u8>),
    SOS(Vec<Component>),
    COM(Vec<u8>),
    Unknown(u8, Vec<u8>),
}

#[derive(Debug)]
struct Component {
    id: u8,
    sampling: u8,
    qt_id: u8,
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JPEG> {
    let (input, segments) = many0(parse_segment)(input)?;
    Ok((input, JPEG { segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = parse_marker(input)?;
    match marker {
        0xFFD8 => Ok((input, Segment::SOI)),
        0xFFD9 => Ok((input, Segment::EOI)),
        0xFFE0..=0xFFEF => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::APP((marker & 0x0F) as u8, data.to_vec())))
        }
        0xFFDB => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::DQT(data.to_vec())))
        }
        0xFFC0 => {
            let (input, length) = be_u16(input)?;
            let (input, precision) = be_u8(input)?;
            let (input, height) = be_u16(input)?;
            let (input, width) = be_u16(input)?;
            let (input, components) = parse_components(input)?;
            Ok((input, Segment::SOF0(length, precision, height, width, components)))
        }
        0xFFC4 => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::DHT(data.to_vec())))
        }
        0xFFDA => {
            let (input, length) = be_u16(input)?;
            let (input, components) = parse_components(input)?;
            Ok((input, Segment::SOS(components)))
        }
        0xFFFE => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::COM(data.to_vec())))
        }
        _ => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::Unknown((marker & 0xFF) as u8, data.to_vec())))
        }
    }
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], u16> {
    let (input, _) = tag([0xFF])(input)?;
    let (input, marker) = be_u8(input)?;
    Ok((input, 0xFF00 | marker as u16))
}

fn parse_components(input: &[u8]) -> IResult<&[u8], Vec<Component>> {
    let (input, num_components) = be_u8(input)?;
    let mut components = Vec::new();
    let mut remaining_input = input;
    for _ in 0..num_components {
        let (input, id) = be_u8(remaining_input)?;
        let (input, sampling) = be_u8(input)?;
        let (input, qt_id) = be_u8(input)?;
        components.push(Component { id, sampling, qt_id });
        remaining_input = input;
    }
    Ok((remaining_input, components))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_jpeg(&buffer) {
        Ok((_, jpeg)) => println!("{:#?}", jpeg),
        Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
    }
}