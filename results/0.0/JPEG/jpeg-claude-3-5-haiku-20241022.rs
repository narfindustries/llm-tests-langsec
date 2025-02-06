use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0, many1},
    number::complete::{be_u8, be_u16},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct JpegMarker(u16);

#[derive(Debug)]
struct App0Segment {
    identifier: Vec<u8>,
    version: (u8, u8),
    density_units: u8,
    x_density: u16,
    y_density: u16,
    thumbnail_x: u8,
    thumbnail_y: u8,
    thumbnail_data: Vec<u8>,
}

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    table_id: u8,
    values: Vec<u16>,
}

#[derive(Debug)]
struct HuffmanTable {
    table_class: u8,
    destination_id: u8,
    code_lengths: Vec<u8>,
    values: Vec<u8>,
}

#[derive(Debug)]
struct StartOfFrame {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<(u8, u8, u8)>,
}

#[derive(Debug)]
struct StartOfScan {
    components: Vec<(u8, u8, u8)>,
    predictor_selection: u8,
    point_transform: u8,
}

#[derive(Debug)]
struct Jpeg {
    soi: JpegMarker,
    app0: Option<App0Segment>,
    quantization_tables: Vec<QuantizationTable>,
    huffman_tables: Vec<HuffmanTable>,
    start_of_frame: Option<StartOfFrame>,
    start_of_scan: Option<StartOfScan>,
    image_data: Vec<u8>,
    eoi: JpegMarker,
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    map(be_u16, JpegMarker)(input)
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], App0Segment> {
    let (input, _length) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;
    let (input, version) = tuple((be_u8, be_u8))(input)?;
    let (input, density_units) = be_u8(input)?;
    let (input, x_density) = be_u16(input)?;
    let (input, y_density) = be_u16(input)?;
    let (input, thumbnail_x) = be_u8(input)?;
    let (input, thumbnail_y) = be_u8(input)?;
    let (input, thumbnail_data) = take((thumbnail_x * thumbnail_y * 3) as usize)(input)?;

    Ok((input, App0Segment {
        identifier: identifier.to_vec(),
        version,
        density_units,
        x_density,
        y_density,
        thumbnail_x,
        thumbnail_y,
        thumbnail_data: thumbnail_data.to_vec(),
    }))
}

fn parse_quantization_table(input: &[u8]) -> IResult<&[u8], QuantizationTable> {
    let (input, _length) = be_u16(input)?;
    let (input, precision_and_id) = be_u8(input)?;
    let precision = (precision_and_id & 0xF0) >> 4;
    let table_id = precision_and_id & 0x0F;
    let (input, values) = count(
        if precision == 0 { 
            map(be_u8, |v| v as u16) 
        } else { 
            map(be_u16, |v| v) 
        },
        64
    )(input)?;

    Ok((input, QuantizationTable {
        precision,
        table_id,
        values,
    }))
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, _length) = be_u16(input)?;
    let (input, class_and_dest) = be_u8(input)?;
    let table_class = (class_and_dest & 0xF0) >> 4;
    let destination_id = class_and_dest & 0x0F;
    let (input, code_lengths) = count(be_u8, 16)(input)?;
    let total_codes: usize = code_lengths.iter().map(|&x| x as usize).sum();
    let (input, values) = take(total_codes)(input)?;

    Ok((input, HuffmanTable {
        table_class,
        destination_id,
        code_lengths,
        values: values.to_vec(),
    }))
}

fn parse_start_of_frame(input: &[u8]) -> IResult<&[u8], StartOfFrame> {
    let (input, _length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, components) = count(
        tuple((be_u8, be_u8, be_u8)),
        num_components as usize
    )(input)?;

    Ok((input, StartOfFrame {
        precision,
        height,
        width,
        components,
    }))
}

fn parse_start_of_scan(input: &[u8]) -> IResult<&[u8], StartOfScan> {
    let (input, _length) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, components) = count(
        tuple((be_u8, be_u8, be_u8)),
        num_components as usize
    )(input)?;
    let (input, predictor_selection) = be_u8(input)?;
    let (input, point_transform) = be_u8(input)?;

    Ok((input, StartOfScan {
        components,
        predictor_selection,
        point_transform,
    }))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Jpeg> {
    let (input, soi) = preceded(tag(&[0xFF, 0xD8]), parse_marker)(input)?;
    let (input, app0) = opt(preceded(tag(&[0xFF, 0xE0]), parse_app0))(input)?;
    let (input, quantization_tables) = many1(preceded(tag(&[0xFF, 0xDB]), parse_quantization_table))(input)?;
    let (input, huffman_tables) = many0(preceded(tag(&[0xFF, 0xC4]), parse_huffman_table))(input)?;
    let (input, start_of_frame) = opt(preceded(tag(&[0xFF, 0xC0]), parse_start_of_frame))(input)?;
    let (input, start_of_scan) = opt(preceded(tag(&[0xFF, 0xDA]), parse_start_of_scan))(input)?;
    let (input, image_data) = take(input.len())(input)?;
    let (input, eoi) = preceded(tag(&[0xFF, 0xD9]), parse_marker)(input)?;

    Ok((input, Jpeg {
        soi,
        app0,
        quantization_tables,
        huffman_tables,
        start_of_frame,
        start_of_scan,
        image_data: image_data.to_vec(),
        eoi,
    }))
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
        }
        Err(e) => {
            eprintln!("Error parsing JPEG: {:?}", e);
            std::process::exit(1)
        }
    }
}