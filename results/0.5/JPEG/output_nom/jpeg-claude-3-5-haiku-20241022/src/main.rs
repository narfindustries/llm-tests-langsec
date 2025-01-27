use nom::{
    bytes::complete::{tag, take, take_while},
    combinator::{map, opt},
    multi::{many0, many1},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{preceded, tuple},
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
    SOF0(SOF0Marker),
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
struct SOF0Marker {
    length: u16,
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<SOF0Component>,
}

#[derive(Debug)]
struct SOF0Component {
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
    lengths: Vec<u8>,
    values: Vec<u8>,
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
    start_spectral_selection: u8,
    end_spectral_selection: u8,
    approximation: u8,
}

#[derive(Debug)]
struct SOSComponent {
    id: u8,
    huffman_tables: u8,
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegFile> {
    let (input, start_of_image) = parse_soi(input)?;
    let (input, segments) = many1(parse_segment)(input)?;
    Ok((input, JpegFile { start_of_image, segments }))
}

fn parse_soi(input: &[u8]) -> IResult<&[u8], u16> {
    let (input, marker) = tag([0xFF, 0xD8])(input)?;
    Ok((input, u16::from_be_bytes([marker[0], marker[1]])))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, marker) = be_u8(input)?;
    match marker {
        0xE0 => map(parse_app0, JpegSegment::APP0)(input),
        0xC0 => map(parse_sof0, JpegSegment::SOF0)(input),
        0xC4 => map(parse_dht, JpegSegment::DHT)(input),
        0xDB => map(parse_dqt, JpegSegment::DQT)(input),
        0xDA => map(parse_sos, JpegSegment::SOS)(input),
        0xD9 => Ok((input, JpegSegment::EOI)),
        _ => take_while(|_| true)(input).map(|(input, _)| (input, JpegSegment::EOI)),
    }
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], APP0Marker> {
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;
    let (input, (version_major, version_minor)) = tuple((be_u8, be_u8))(input)?;
    let (input, density_units) = be_u8(input)?;
    let (input, x_density) = be_u16(input)?;
    let (input, y_density) = be_u16(input)?;
    let (input, thumbnail_width) = be_u8(input)?;
    let (input, thumbnail_height) = be_u8(input)?;
    let (input, thumbnail_data) = take((thumbnail_width * thumbnail_height * 3) as usize)(input)?;

    Ok((input, APP0Marker {
        length,
        identifier: identifier.to_vec(),
        version: (version_major, version_minor),
        density_units,
        x_density,
        y_density,
        thumbnail_width,
        thumbnail_height,
        thumbnail_data: thumbnail_data.to_vec(),
    }))
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], SOF0Marker> {
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let (input, components) = many1(parse_sof0_component)(input)?;

    Ok((input, SOF0Marker {
        length,
        precision,
        height,
        width,
        components,
    }))
}

fn parse_sof0_component(input: &[u8]) -> IResult<&[u8], SOF0Component> {
    let (input, id) = be_u8(input)?;
    let (input, sampling_factors) = be_u8(input)?;
    let (input, quantization_table_id) = be_u8(input)?;

    Ok((input, SOF0Component {
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
    let (input, lengths) = take(16usize)(input)?;
    let total_values: usize = lengths.iter().map(|&x| x as usize).sum();
    let (input, values) = take(total_values)(input)?;

    Ok((input, DHTTable {
        class,
        destination_id,
        lengths: lengths.to_vec(),
        values: values.to_vec(),
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
    let table_size = if precision == 0 { 64 } else { 128 };
    let (input, values) = take(table_size)(input)?;

    Ok((input, DQTTable {
        precision,
        id,
        values: values.to_vec(),
    }))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], SOSMarker> {
    let (input, length) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let (input, components) = many1(parse_sos_component)(input)?;
    let (input, start_spectral_selection) = be_u8(input)?;
    let (input, end_spectral_selection) = be_u8(input)?;
    let (input, approximation) = be_u8(input)?;

    Ok((input, SOSMarker {
        length,
        components,
        start_spectral_selection,
        end_spectral_selection,
        approximation,
    }))
}

fn parse_sos_component(input: &[u8]) -> IResult<&[u8], SOSComponent> {
    let (input, id) = be_u8(input)?;
    let (input, huffman_tables) = be_u8(input)?;

    Ok((input, SOSComponent { id, huffman_tables }))
}

fn main() -> std::io::Result<()> {
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
            println!("Successfully parsed JPEG: {:?}", jpeg);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse JPEG: {:?}", e);
            std::process::exit(1);
        }
    }
}