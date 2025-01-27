use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::many0,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct SOF0 {
    precision: u8,
    height: u16,
    width: u16,
    num_components: u8,
    components: Vec<Component>,
}

#[derive(Debug)]
struct Component {
    id: u8,
    sampling_factor: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
enum Segment {
    SOI,
    EOI,
    SOF0(SOF0),
    DQT(Vec<u8>),
    DHT,
    SOS,
    APPn(Vec<u8>),
    Unknown(Vec<u8>),
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], SOF0> {
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, components) = many0(parse_component)(input)?;
    Ok((input, SOF0 {
        precision,
        height,
        width,
        num_components,
        components,
    }))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, id) = be_u8(input)?;
    let (input, sampling_factor) = be_u8(input)?;
    let (input, quantization_table_id) = be_u8(input)?;
    Ok((input, Component {
        id,
        sampling_factor,
        quantization_table_id,
    }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = be_u16(input)?;
    match marker {
        0xFFD8 => Ok((input, Segment::SOI)),
        0xFFD9 => Ok((input, Segment::EOI)),
        0xFFC0 => {
            let (input, _length) = be_u16(input)?;
            let (input, sof0) = parse_sof0(input)?;
            Ok((input, Segment::SOF0(sof0)))
        },
        0xFFDB => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::DQT(data.to_vec())))
        },
        0xFFC4 => Ok((input, Segment::DHT)),
        0xFFDA => Ok((input, Segment::SOS)),
        0xFFE0..=0xFFEF => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::APPn(data.to_vec())))
        },
        _ => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::Unknown(data.to_vec())))
        },
    }
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Vec<Segment>> {
    many0(parse_segment)(input)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <JPEG_FILE>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_jpeg(&buffer) {
        Ok((_, segments)) => {
            for segment in segments {
                println!("{:?}", segment);
            }
        },
        Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
    }

    Ok(())
}