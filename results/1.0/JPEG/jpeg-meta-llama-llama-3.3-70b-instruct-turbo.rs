use nom::{
    bytes::complete::{tag, take},
    combinator::opt,
    error::{ErrorKind, ParseError},
    number::complete::{be_u8, be_u16},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

// Marker
#[derive(Debug, PartialEq)]
enum Marker {
    SOF,
    DHT,
    SOS,
    EOI,
    APP0,
    APP1,
    APP2,
    APP3,
    APP4,
    APP5,
    APP6,
    APP7,
    APP8,
    APP9,
    APP10,
    APP11,
    APP12,
    APP13,
    APP14,
    APP15,
}

fn marker(input: &[u8]) -> IResult<&[u8], Marker> {
    let (input, marker_byte) = be_u8(input)?;
    match marker_byte {
        0xC0 => Ok((input, Marker::SOF)),
        0xC4 => Ok((input, Marker::DHT)),
        0xDA => Ok((input, Marker::SOS)),
        0xD9 => Ok((input, Marker::EOI)),
        0xE0 => Ok((input, Marker::APP0)),
        0xE1 => Ok((input, Marker::APP1)),
        0xE2 => Ok((input, Marker::APP2)),
        0xE3 => Ok((input, Marker::APP3)),
        0xE4 => Ok((input, Marker::APP4)),
        0xE5 => Ok((input, Marker::APP5)),
        0xE6 => Ok((input, Marker::APP6)),
        0xE7 => Ok((input, Marker::APP7)),
        0xE8 => Ok((input, Marker::APP8)),
        0xE9 => Ok((input, Marker::APP9)),
        0xEA => Ok((input, Marker::APP10)),
        0xEB => Ok((input, Marker::APP11)),
        0xEC => Ok((input, Marker::APP12)),
        0xED => Ok((input, Marker::APP13)),
        0xEE => Ok((input, Marker::APP14)),
        0xEF => Ok((input, Marker::APP15)),
        _ => Err(nom::Err::Error(nom::error::Error::from_error_kind(input, ErrorKind::AlphaNumeric))),
    }
}

// Frame Header
#[derive(Debug, PartialEq)]
struct FrameHeader {
    precision: u8,
    image_height: u16,
    image_width: u16,
    num_components: u8,
    component_identifier: u8,
}

fn frame_header(input: &[u8]) -> IResult<&[u8], FrameHeader> {
    let (input, precision) = be_u8(input)?;
    let (input, image_height) = be_u16(input)?;
    let (input, image_width) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, component_identifier) = be_u8(input)?;
    Ok((
        input,
        FrameHeader {
            precision,
            image_height,
            image_width,
            num_components,
            component_identifier,
        },
    ))
}

// Huffman Table
#[derive(Debug, PartialEq)]
struct HuffmanTable {
    table_class: u8,
    table_identifier: u8,
    num_codes: u16,
    code_lengths: Vec<u8>,
}

fn huffman_table(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, table_class) = be_u8(input)?;
    let (input, table_identifier) = be_u8(input)?;
    let (input, num_codes) = be_u16(input)?;
    let (input, code_lengths) = take(num_codes as usize)(input)?;
    let code_lengths = code_lengths.to_vec();
    Ok((
        input,
        HuffmanTable {
            table_class,
            table_identifier,
            num_codes,
            code_lengths,
        },
    ))
}

// Scan Header
#[derive(Debug, PartialEq)]
struct ScanHeader {
    num_components: u8,
    component_identifier: u8,
    dc_huffman_table: u8,
    ac_huffman_table: u8,
}

fn scan_header(input: &[u8]) -> IResult<&[u8], ScanHeader> {
    let (input, num_components) = be_u8(input)?;
    let (input, component_identifier) = be_u8(input()?;
    let (input, dc_huffman_table) = be_u8(input)?;
    let (input, ac_huffman_table) = be_u8(input)?;
    Ok((
        input,
        ScanHeader {
            num_components,
            component_identifier,
            dc_huffman_table,
            ac_huffman_table,
        },
    ))
}

// APP0 Segment
#[derive(Debug, PartialEq)]
struct App0Segment {
    identifier: [u8; 5],
    version: u8,
    units: u8,
    x_density: u16,
    y_density: u16,
    thumbnail_height: u8,
    thumbnail_width: u8,
}

fn app0_segment(input: &[u8]) -> IResult<&[u8], App0Segment> {
    let (input, identifier) = take(5u8)(input)?;
    let identifier = [
        identifier[0],
        identifier[1],
        identifier[2],
        identifier[3],
        identifier[4],
    ];
    let (input, version) = be_u8(input)?;
    let (input, units) = be_u8(input)?;
    let (input, x_density) = be_u16(input)?;
    let (input, y_density) = be_u16(input)?;
    let (input, thumbnail_height) = be_u8(input)?;
    let (input, thumbnail_width) = be_u8(input)?;
    Ok((
        input,
        App0Segment {
            identifier,
            version,
            units,
            x_density,
            y_density,
            thumbnail_height,
            thumbnail_width,
        },
    ))
}

// JPEG Parser
fn jpeg_parser(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0xFF, 0xD8])(input)?; // SOI
    let (input, _) = opt(app0_segment)(input)?; // APP0
    let (input, _) = opt(huffman_table)(input)?; // DHT
    let (input, _) = frame_header(input)?; // SOF
    let (input, _) = scan_header(input)?; // SOS
    let (input, _) = tag(&[0xFF, 0xD9])(input)?; // EOI
    Ok((input, ()))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <image_file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");
    match jpeg_parser(&buffer) {
        Ok((_, ())) => println!("JPEG file parsed successfully"),
        Err(e) => println!("Error parsing JPEG file: {:?}", e),
    }
}