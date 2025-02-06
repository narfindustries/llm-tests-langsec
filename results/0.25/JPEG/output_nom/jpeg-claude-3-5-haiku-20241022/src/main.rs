use nom::{
    bytes::complete::{tag, take},
    multi::{count, many0},
    number::complete::{be_u8, be_u16},
    sequence::{tuple, preceded},
    IResult,
    branch::alt,
    combinator::{map, opt},
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct JpegMarker(u16);

#[derive(Debug)]
struct App0Segment {
    length: u16,
    identifier: Vec<u8>,
    version: u16,
    density_units: u8,
    x_density: u16,
    y_density: u16,
    thumbnail_width: u8,
    thumbnail_height: u8,
}

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    table_id: u8,
    matrix: Vec<u8>,
}

#[derive(Debug)]
struct HuffmanTable {
    table_class: u8,
    table_destination: u8,
    code_lengths: Vec<u8>,
    values: Vec<u8>,
}

#[derive(Debug)]
struct StartOfFrame {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<Component>,
}

#[derive(Debug)]
struct Component {
    id: u8,
    sampling_factors: u8,
    quantization_table: u8,
}

#[derive(Debug)]
struct ScanComponent {
    selector: u8,
    dc_huffman_table: u8,
    ac_huffman_table: u8,
}

#[derive(Debug)]
struct ScanHeader {
    components: Vec<ScanComponent>,
    spectral_start: u8,
    spectral_end: u8,
    approximation: u8,
}

#[derive(Debug)]
struct Jpeg {
    app0: Option<App0Segment>,
    quantization_tables: Vec<QuantizationTable>,
    huffman_tables: Vec<HuffmanTable>,
    start_of_frame: Option<StartOfFrame>,
    scan_header: Option<ScanHeader>,
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    map(be_u16, JpegMarker)(input)
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], App0Segment> {
    let (input, (length, identifier, version, density_units, x_density, y_density, thumbnail_width, thumbnail_height)) = tuple((
        be_u16,
        take(5usize),
        be_u16,
        be_u8,
        be_u16,
        be_u16,
        be_u8,
        be_u8,
    ))(input)?;

    Ok((input, App0Segment {
        length,
        identifier: identifier.to_vec(),
        version,
        density_units,
        x_density,
        y_density,
        thumbnail_width,
        thumbnail_height,
    }))
}

fn parse_quantization_table(input: &[u8]) -> IResult<&[u8], QuantizationTable> {
    let (input, (precision, table_id)) = tuple((be_u8, be_u8))(input)?;
    let matrix_size: usize = if precision == 0 { 64 } else { 128 };
    let (input, matrix) = take(matrix_size)(input)?;

    Ok((input, QuantizationTable {
        precision,
        table_id,
        matrix: matrix.to_vec(),
    }))
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, (table_class, table_destination)) = tuple((be_u8, be_u8))(input)?;
    let (input, code_lengths) = take(16usize)(input)?;
    let total_codes: usize = code_lengths.iter().map(|&x| x as usize).sum();
    let (input, values) = take(total_codes)(input)?;

    Ok((input, HuffmanTable {
        table_class,
        table_destination,
        code_lengths: code_lengths.to_vec(),
        values: values.to_vec(),
    }))
}

fn parse_start_of_frame(input: &[u8]) -> IResult<&[u8], StartOfFrame> {
    let (input, (precision, height, width, component_count)) = tuple((be_u8, be_u16, be_u16, be_u8))(input)?;
    let (input, components) = count(parse_component, component_count as usize)(input)?;

    Ok((input, StartOfFrame {
        precision,
        height,
        width,
        components,
    }))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, (id, sampling_factors, quantization_table)) = tuple((be_u8, be_u8, be_u8))(input)?;

    Ok((input, Component {
        id,
        sampling_factors,
        quantization_table,
    }))
}

fn parse_scan_header(input: &[u8]) -> IResult<&[u8], ScanHeader> {
    let (input, component_count) = be_u8(input)?;
    let (input, components) = count(parse_scan_component, component_count as usize)(input)?;
    let (input, (spectral_start, spectral_end, approximation)) = tuple((be_u8, be_u8, be_u8))(input)?;

    Ok((input, ScanHeader {
        components,
        spectral_start,
        spectral_end,
        approximation,
    }))
}

fn parse_scan_component(input: &[u8]) -> IResult<&[u8], ScanComponent> {
    let (input, (selector, huffman_tables)) = tuple((be_u8, be_u8))(input)?;

    Ok((input, ScanComponent {
        selector,
        dc_huffman_table: huffman_tables >> 4,
        ac_huffman_table: huffman_tables & 0x0F,
    }))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Jpeg> {
    let (input, _) = tag([0xFF, 0xD8])(input)?;

    let (input, app0) = opt(preceded(tag([0xFF, 0xE0]), parse_app0))(input)?;
    let (input, quantization_tables) = many0(preceded(tag([0xFF, 0xDB]), parse_quantization_table))(input)?;
    let (input, huffman_tables) = many0(preceded(tag([0xFF, 0xC4]), parse_huffman_table))(input)?;

    let (input, start_of_frame) = opt(alt((
        preceded(tag([0xFF, 0xC0]), parse_start_of_frame),
        preceded(tag([0xFF, 0xC2]), parse_start_of_frame)
    )))(input)?;

    let (input, scan_header) = opt(preceded(tag([0xFF, 0xDA]), parse_scan_header))(input)?;

    let (input, _) = tag([0xFF, 0xD9])(input)?;

    Ok((input, Jpeg {
        app0,
        quantization_tables,
        huffman_tables,
        start_of_frame,
        scan_header,
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
            std::process::exit(1);
        }
    }
}