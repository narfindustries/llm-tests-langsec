use nom::{
    bytes::complete::{tag, take, take_while_m_n},
    combinator::{map, map_res, opt},
    error::{ErrorKind, VerboseError},
    multi::take_while_m_n,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{pair, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug, PartialEq)]
enum JpegMarker {
    SOI,
    DQT,
    DHT,
    SOF0,
    DRI,
    SOS,
    EOI,
    Other(u8),
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    let (input, byte1) = take(1u8)(input)?;
    let (input, byte2) = take(1u8)(input)?;

    match (byte1[0], byte2[0]) {
        (0xFF, 0xD8) => Ok((input, JpegMarker::SOI)),
        (0xFF, 0xDB) => Ok((input, JpegMarker::DQT)),
        (0xFF, 0xC4) => Ok((input, JpegMarker::DHT)),
        (0xFF, 0xC0) => Ok((input, JpegMarker::SOF0)),
        (0xFF, 0xDD) => Ok((input, JpegMarker::DRI)),
        (0xFF, 0xDA) => Ok((input, JpegMarker::SOS)),
        (0xFF, 0xD9) => Ok((input, JpegMarker::EOI)),
        (0xFF, other) => Ok((input, JpegMarker::Other(other))),
        _ => Err(nom::Err::Error((input, ErrorKind::AlphaNumeric))),
    }
}

#[derive(Debug, PartialEq)]
struct DqtSegment {
    precision: u8,
    length: u16,
    table: Vec<u8>,
}

fn parse_dqt_segment(input: &[u8]) -> IResult<&[u8], DqtSegment> {
    let (input, precision) = be_u8(input)?;
    let (input, length) = be_u16(input)?;
    let (input, table) = take(length as usize - 2)(input)?;
    Ok((input, DqtSegment { precision, length, table: table.to_vec() }))
}

#[derive(Debug, PartialEq)]
struct DhtSegment {
    class: u8,
    destination: u8,
    length: u16,
    table: Vec<u8>,
}

fn parse_dht_segment(input: &[u8]) -> IResult<&[u8], DhtSegment> {
    let (input, class) = be_u8(input)?;
    let (input, destination) = be_u8(input)?;
    let (input, length) = be_u16(input)?;
    let (input, table) = take(length as usize - 2)(input)?;
    Ok((input, DhtSegment { class, destination, length, table: table.to_vec() }))
}

#[derive(Debug, PartialEq)]
struct Sof0Segment {
    precision: u8,
    height: u16,
    width: u16,
    component_count: u8,
    components: Vec<(u8, u8, u8)>,
}

fn parse_sof0_segment(input: &[u8]) -> IResult<&[u8], Sof0Segment> {
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let (input, components) = take(component_count as usize * 3)(input)?;
    let components: Vec<(u8, u8, u8)> = components
        .chunks(3)
        .map(|c| (c[0], c[1], c[2]))
        .collect();
    Ok((input, Sof0Segment { precision, height, width, component_count, components }))
}

#[derive(Debug, PartialEq)]
struct DriSegment {
    restart_interval: u16,
}

fn parse_dri_segment(input: &[u8]) -> IResult<&[u8], DriSegment> {
    let (input, restart_interval) = be_u16(input)?;
    Ok((input, DriSegment { restart_interval }))
}

#[derive(Debug, PartialEq)]
struct SosSegment {
    component_count: u8,
    components: Vec<(u8, u8, u8)>,
    start_spectral: u8,
    end_spectral: u8,
    successive_approximation: u8,
}

fn parse_sos_segment(input: &[u8]) -> IResult<&[u8], SosSegment> {
    let (input, component_count) = be_u8(input)?;
    let (input, components) = take(component_count as usize * 2)(input)?;
    let components: Vec<(u8, u8, u8)> = components
        .chunks(2)
        .map(|c| (c[0], c[1], 0))
        .collect();
    let (input, start_spectral) = be_u8(input)?;
    let (input, end_spectral) = be_u8(input)?;
    let (input, successive_approximation) = be_u8(input)?;
    Ok((input, SosSegment { component_count, components, start_spectral, end_spectral, successive_approximation }))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = parse_marker(input)?;
    let mut input = input;
    loop {
        match parse_marker(input) {
            Ok((i, JpegMarker::EOI)) => {
                input = i;
                break;
            }
            Ok((i, JpegMarker::DQT)) => {
                let (i, _) = parse_dqt_segment(i)?;
                input = i;
            }
            Ok((i, JpegMarker::DHT)) => {
                let (i, _) = parse_dht_segment(i)?;
                input = i;
            }
            Ok((i, JpegMarker::SOF0)) => {
                let (i, _) = parse_sof0_segment(i)?;
                input = i;
            }
            Ok((i, JpegMarker::DRI)) => {
                let (i, _) = parse_dri_segment(i)?;
                input = i;
            }
            Ok((i, JpegMarker::SOS)) => {
                let (i, _) = parse_sos_segment(i)?;
                input = i;
            }
            _ => break,
        }
    }
    Ok((input, ()))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <jpeg_file>", args[0]);
        return;
    }
    let path = Path::new(&args[1]);
    let mut file = File::open(path).unwrap();
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).unwrap();
    match parse_jpeg(&buffer) {
        Ok((_, ())) => println!("Parsed JPEG successfully"),
        Err(e) => println!("Error parsing JPEG: {:?}", e),
    }
}