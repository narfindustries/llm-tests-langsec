use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::tuple,
    IResult,
    multi::many0,
    combinator::map,
    branch::alt,
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
    APP0(APP0),
    APPn(u8, Vec<u8>),
    SOF0(SOF0),
    DHT(DHT),
    DQT(DQT),
    SOS(SOS),
    COM(Vec<u8>),
    Unknown(u8, Vec<u8>),
}

#[derive(Debug)]
struct APP0 {
    identifier: Vec<u8>,
    version: [u8; 2],
    units: u8,
    x_density: u16,
    y_density: u16,
    thumbnail_width: u8,
    thumbnail_height: u8,
    thumbnail_data: Vec<u8>,
}

#[derive(Debug)]
struct SOF0 {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<Component>,
}

#[derive(Debug)]
struct Component {
    id: u8,
    sampling: u8,
    quantization_table: u8,
}

#[derive(Debug)]
struct DHT {
    tables: Vec<HuffmanTable>,
}

#[derive(Debug)]
struct HuffmanTable {
    class: u8,
    destination: u8,
    codes: Vec<u8>,
    values: Vec<u8>,
}

#[derive(Debug)]
struct DQT {
    tables: Vec<QuantizationTable>,
}

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    destination: u8,
    values: Vec<u8>,
}

#[derive(Debug)]
struct SOS {
    components: Vec<SOSComponent>,
    spectral_start: u8,
    spectral_end: u8,
    successive_approximation: u8,
}

#[derive(Debug)]
struct SOSComponent {
    id: u8,
    dc_table: u8,
    ac_table: u8,
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
        0xFFE0 => map(parse_app0, Segment::APP0)(input),
        0xFFC0 => map(parse_sof0, Segment::SOF0)(input),
        0xFFC4 => map(parse_dht, Segment::DHT)(input),
        0xFFDB => map(parse_dqt, Segment::DQT)(input),
        0xFFDA => map(parse_sos, Segment::SOS)(input),
        0xFFFE => map(parse_com, Segment::COM)(input),
        _ => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::Unknown((marker >> 8) as u8, data.to_vec())))
        }
    }
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], u16> {
    let (input, marker) = be_u16(input)?;
    Ok((input, marker))
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], APP0> {
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;
    let (input, version) = take(2usize)(input)?;
    let (input, units) = be_u8(input)?;
    let (input, x_density) = be_u16(input)?;
    let (input, y_density) = be_u16(input)?;
    let (input, thumbnail_width) = be_u8(input)?;
    let (input, thumbnail_height) = be_u8(input)?;
    let (input, thumbnail_data) = take((thumbnail_width as usize) * (thumbnail_height as usize) * 3)(input)?;
    Ok((input, APP0 {
        identifier: identifier.to_vec(),
        version: [version[0], version[1]],
        units,
        x_density,
        y_density,
        thumbnail_width,
        thumbnail_height,
        thumbnail_data: thumbnail_data.to_vec(),
    }))
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], SOF0> {
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let (input, components) = many0(parse_component)(input)?;
    Ok((input, SOF0 {
        precision,
        height,
        width,
        components,
    }))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, id) = be_u8(input)?;
    let (input, sampling) = be_u8(input)?;
    let (input, quantization_table) = be_u8(input)?;
    Ok((input, Component {
        id,
        sampling,
        quantization_table,
    }))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], DHT> {
    let (input, length) = be_u16(input)?;
    let (input, tables) = many0(parse_huffman_table)(input)?;
    Ok((input, DHT { tables }))
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, (class, destination)) = tuple((be_u8, be_u8))(input)?;
    let (input, codes) = take(16usize)(input)?;
    let (input, values) = take(256usize)(input)?;
    Ok((input, HuffmanTable {
        class,
        destination,
        codes: codes.to_vec(),
        values: values.to_vec(),
    }))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], DQT> {
    let (input, length) = be_u16(input)?;
    let (input, tables) = many0(parse_quantization_table)(input)?;
    Ok((input, DQT { tables }))
}

fn parse_quantization_table(input: &[u8]) -> IResult<&[u8], QuantizationTable> {
    let (input, (precision, destination)) = tuple((be_u8, be_u8))(input)?;
    let (input, values) = take(64usize)(input)?;
    Ok((input, QuantizationTable {
        precision,
        destination,
        values: values.to_vec(),
    }))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], SOS> {
    let (input, length) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let (input, components) = many0(parse_sos_component)(input)?;
    let (input, spectral_start) = be_u8(input)?;
    let (input, spectral_end) = be_u8(input)?;
    let (input, successive_approximation) = be_u8(input)?;
    Ok((input, SOS {
        components,
        spectral_start,
        spectral_end,
        successive_approximation,
    }))
}

fn parse_sos_component(input: &[u8]) -> IResult<&[u8], SOSComponent> {
    let (input, id) = be_u8(input)?;
    let (input, (dc_table, ac_table)) = tuple((be_u8, be_u8))(input)?;
    Ok((input, SOSComponent {
        id,
        dc_table,
        ac_table,
    }))
}

fn parse_com(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length - 2)(input)?;
    Ok((input, data.to_vec()))
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