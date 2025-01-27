use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0, many1},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct JpegFile {
    start_of_image: u16,
    segments: Vec<JpegSegment>,
}

#[derive(Debug)]
enum JpegSegment {
    APP0(APP0Marker),
    SOFn(SOFMarker),
    DHT(DHTMarker),
    DQT(DQTMarker),
    SOS(SOSMarker),
    EOI,
}

#[derive(Debug)]
struct APP0Marker {
    length: u16,
    identifier: Vec<u8>,
    version: (u8, u8),
    density_units: u8,
    x_density: u16,
    y_density: u16,
    thumbnail_width: u8,
    thumbnail_height: u8,
    thumbnail_data: Vec<u8>,
}

#[derive(Debug)]
struct SOFMarker {
    length: u16,
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<SOFComponent>,
}

#[derive(Debug)]
struct SOFComponent {
    id: u8,
    sampling_factors: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct DHTMarker {
    length: u16,
    tables: Vec<DHTTable>,
}

#[derive(Debug)]
struct DHTTable {
    class: u8,
    destination_id: u8,
    huffman_bits: Vec<u8>,
    huffman_values: Vec<u8>,
}

#[derive(Debug)]
struct DQTMarker {
    length: u16,
    tables: Vec<DQTTable>,
}

#[derive(Debug)]
struct DQTTable {
    precision: u8,
    id: u8,
    values: Vec<u8>,
}

#[derive(Debug)]
struct SOSMarker {
    length: u16,
    components: Vec<SOSComponent>,
    spectral_selection_start: u8,
    spectral_selection_end: u8,
    approximation: u8,
}

#[derive(Debug)]
struct SOSComponent {
    id: u8,
    huffman_tables: u8,
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegFile> {
    let (input, start_of_image) = be_u16(input)?;
    let (input, segments) = many1(parse_segment)(input)?;
    Ok((input, JpegFile { start_of_image, segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, marker) = be_u8(input)?;
    match marker {
        0xFF => {
            let (input, segment_type) = be_u8(input)?;
            match segment_type {
                0xE0 => map(parse_app0, JpegSegment::APP0)(input),
                0xC0 | 0xC1 | 0xC2 | 0xC3 | 0xC5 | 0xC6 | 0xC7 | 0xC9 | 0xCA | 0xCB | 0xCD | 0xCE | 0xCF => 
                    map(parse_sof, JpegSegment::SOFn)(input),
                0xC4 => map(parse_dht, JpegSegment::DHT)(input),
                0xDB => map(parse_dqt, JpegSegment::DQT)(input),
                0xDA => map(parse_sos, JpegSegment::SOS)(input),
                0xD9 => Ok((input, JpegSegment::EOI)),
                _ => take(0usize)(input),
            }
        },
        _ => take(0usize)(input),
    }
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], APP0Marker> {
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;
    let (input, version) = tuple((be_u8, be_u8))(input)?;
    let (input, density_units) = be_u8(input)?;
    let (input, x_density) = be_u16(input)?;
    let (input, y_density) = be_u16(input)?;
    let (input, thumbnail_width) = be_u8(input)?;
    let (input, thumbnail_height) = be_u8(input)?;
    let (input, thumbnail_data) = take((thumbnail_width * thumbnail_height * 3) as usize)(input)?;

    Ok((input, APP0Marker {
        length,
        identifier: identifier.to_vec(),
        version,
        density_units,
        x_density,
        y_density,
        thumbnail_width,
        thumbnail_height,
        thumbnail_data: thumbnail_data.to_vec(),
    }))
}

fn parse_sof(input: &[u8]) -> IResult<&[u8], SOFMarker> {
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let (input, components) = count(parse_sof_component, component_count as usize)(input)?;

    Ok((input, SOFMarker {
        length,
        precision,
        height,
        width,
        components,
    }))
}

fn parse_sof_component(input: &[u8]) -> IResult<&[u8], SOFComponent> {
    let (input, id) = be_u8(input)?;
    let (input, sampling_factors) = be_u8(input)?;
    let (input, quantization_table_id) = be_u8(input)?;

    Ok((input, SOFComponent {
        id,
        sampling_factors,
        quantization_table_id,
    }))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], DHTMarker> {
    let (input, length) = be_u16(input)?;
    let (input, tables) = many1(parse_dht_table)(input)?;

    Ok((input, DHTMarker { length, tables }))
}

fn parse_dht_table(input: &[u8]) -> IResult<&[u8], DHTTable> {
    let (input, class) = be_u8(input)?;
    let (input, destination_id) = be_u8(input)?;
    let (input, huffman_bits) = take(16usize)(input)?;
    let huffman_values_count: usize = huffman_bits.iter().map(|&x| x as usize).sum();
    let (input, huffman_values) = take(huffman_values_count)(input)?;

    Ok((input, DHTTable {
        class,
        destination_id,
        huffman_bits: huffman_bits.to_vec(),
        huffman_values: huffman_values.to_vec(),
    }))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], DQTMarker> {
    let (input, length) = be_u16(input)?;
    let (input, tables) = many1(parse_dqt_table)(input)?;

    Ok((input, DQTMarker { length, tables }))
}

fn parse_dqt_table(input: &[u8]) -> IResult<&[u8], DQTTable> {
    let (input, precision_and_id) = be_u8(input)?;
    let precision = (precision_and_id & 0xF0) >> 4;
    let id = precision_and_id & 0x0F;
    let values_count = if precision == 0 { 64 } else { 128 };
    let (input, values) = take(values_count)(input)?;

    Ok((input, DQTTable {
        precision,
        id,
        values: values.to_vec(),
    }))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], SOSMarker> {
    let (input, length) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let (input, components) = count(parse_sos_component, component_count as usize)(input)?;
    let (input, spectral_selection_start) = be_u8(input)?;
    let (input, spectral_selection_end) = be_u8(input)?;
    let (input, approximation) = be_u8(input)?;

    Ok((input, SOSMarker {
        length,
        components,
        spectral_selection_start,
        spectral_selection_end,
        approximation,
    }))
}

fn parse_sos_component(input: &[u8]) -> IResult<&[u8], SOSComponent> {
    let (input, id) = be_u8(input)?;
    let (input, huffman_tables) = be_u8(input)?;

    Ok((input, SOSComponent { id, huffman_tables }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <jpeg_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_jpeg(&buffer) {
        Ok((_, jpeg)) => {
            println!("Parsed JPEG: {:?}", jpeg);
            Ok(())
        },
        Err(e) => {
            eprintln!("Error parsing JPEG: {:?}", e);
            std::process::exit(1);
        }
    }
}