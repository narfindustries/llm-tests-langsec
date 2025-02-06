use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{count, many0},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

#[derive(Debug)]
struct JPEG {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    SOI,
    EOI,
    APP(Vec<u8>),
    DQT(u8, Vec<u8>), // Simplified
    SOF(u16, u16, u8, Vec<Component>),
    DHT(u8, Vec<(u8, u8)>), // Simplified
    SOS(Vec<ComponentSelector>),
    Unknown(Vec<u8>),
}

#[derive(Debug)]
struct Component {
    id: u8,
    sampling_factor: u8,
    quant_table_id: u8,
}

#[derive(Debug)]
struct ComponentSelector {
    id: u8,
    dc_table_id: u8,
    ac_table_id: u8,
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JPEG> {
    let (input, segments) = many0(parse_segment)(input)?;
    Ok((input, JPEG { segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = be_u16(input)?;
    match marker {
        0xFFD8 => Ok((input, Segment::SOI)),
        0xFFD9 => Ok((input, Segment::EOI)),
        0xFFE0..=0xFFEF => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::APP(data.to_vec())))
        }
        0xFFDB => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::DQT(0, data.to_vec()))) // Simplified
        }
        0xFFC0 => {
            let (input, length) = be_u16(input)?;
            let (input, (precision, height, width, num_components)) = tuple((be_u8, be_u16, be_u16, be_u8))(input)?;
            let (input, components) = count(parse_component, num_components as usize)(input)?;
            Ok((input, Segment::SOF(height, width, precision, components)))
        }
        0xFFC4 => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::DHT(0, vec![]))) // Simplified
        }
        0xFFDA => {
            let (input, length) = be_u16(input)?;
            let (input, num_components) = be_u8(input)?;
            let (input, selectors) = count(parse_component_selector, num_components as usize)(input)?;
            let (input, _) = take(3usize)(input)?; // Skip 3 bytes
            Ok((input, Segment::SOS(selectors)))
        }
        _ => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::Unknown(data.to_vec())))
        }
    }
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, (id, sampling_factor, quant_table_id)) = tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((input, Component { id, sampling_factor, quant_table_id }))
}

fn parse_component_selector(input: &[u8]) -> IResult<&[u8], ComponentSelector> {
    let (input, (id, dc_table_id, ac_table_id)) = tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((input, ComponentSelector { id, dc_table_id, ac_table_id }))
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <JPEG_FILE>", args[0]);
        return;
    }

    let data = read_file(&args[1]).expect("Failed to read file");
    match parse_jpeg(&data) {
        Ok((_, jpeg)) => println!("{:#?}", jpeg),
        Err(e) => println!("Failed to parse JPEG: {:?}", e),
    }
}