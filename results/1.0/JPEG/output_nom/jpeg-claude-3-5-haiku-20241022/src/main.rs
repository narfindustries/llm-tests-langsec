use nom::{
    bytes::complete::{tag, take, take_while},
    combinator::map,
    multi::{count, many0},
    number::complete::{be_u8, be_u16},
    sequence::preceded,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct JpegMarker(u16);

#[derive(Debug)]
struct JpegHeader {
    soi: JpegMarker,
    app_markers: Vec<AppMarker>,
    quantization_tables: Vec<QuantizationTable>,
    huffman_tables: Vec<HuffmanTable>,
    frame_header: FrameHeader,
    scan_headers: Vec<ScanHeader>,
    compressed_data: Vec<u8>,
    eoi: JpegMarker,
}

#[derive(Debug)]
struct AppMarker {
    marker: JpegMarker,
    length: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct QuantizationTable {
    marker: JpegMarker,
    length: u16,
    precision: u8,
    table_id: u8,
    matrix: Vec<u8>,
}

#[derive(Debug)]
struct HuffmanTable {
    marker: JpegMarker,
    length: u16,
    table_type: u8,
    table_id: u8,
    codes: Vec<u8>,
}

#[derive(Debug)]
struct FrameHeader {
    marker: JpegMarker,
    length: u16,
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<FrameComponent>,
}

#[derive(Debug)]
struct FrameComponent {
    id: u8,
    sampling_factors: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct ScanHeader {
    marker: JpegMarker,
    length: u16,
    component_count: u8,
    components: Vec<ScanComponent>,
    spectral_start: u8,
    spectral_end: u8,
    approximation: u8,
}

#[derive(Debug)]
struct ScanComponent {
    id: u8,
    huffman_tables: u8,
}

fn parse_jpeg_marker(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    map(be_u16, JpegMarker)(input)
}

fn parse_app_marker(input: &[u8]) -> IResult<&[u8], AppMarker> {
    let (input, marker) = parse_jpeg_marker(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take((length - 2) as usize)(input)?;
    Ok((input, AppMarker { marker, length, data: data.to_vec() }))
}

fn parse_quantization_table(input: &[u8]) -> IResult<&[u8], QuantizationTable> {
    let (input, marker) = parse_jpeg_marker(input)?;
    let (input, length) = be_u16(input)?;
    let (input, precision_and_table_id) = be_u8(input)?;
    let precision = (precision_and_table_id >> 4) & 0x0F;
    let table_id = precision_and_table_id & 0x0F;
    let (input, matrix) = take(64usize)(input)?;
    Ok((input, QuantizationTable {
        marker,
        length,
        precision,
        table_id,
        matrix: matrix.to_vec(),
    }))
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, marker) = parse_jpeg_marker(input)?;
    let (input, length) = be_u16(input)?;
    let (input, table_info) = be_u8(input)?;
    let table_type = (table_info >> 4) & 0x0F;
    let table_id = table_info & 0x0F;
    let (input, codes) = take((length - 3) as usize)(input)?;
    Ok((input, HuffmanTable {
        marker,
        length,
        table_type,
        table_id,
        codes: codes.to_vec(),
    }))
}

fn parse_frame_header(input: &[u8]) -> IResult<&[u8], FrameHeader> {
    let (input, marker) = parse_jpeg_marker(input)?;
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let (input, components) = count(parse_frame_component, component_count as usize)(input)?;
    Ok((input, FrameHeader {
        marker,
        length,
        precision,
        height,
        width,
        components,
    }))
}

fn parse_frame_component(input: &[u8]) -> IResult<&[u8], FrameComponent> {
    let (input, id) = be_u8(input)?;
    let (input, sampling_factors) = be_u8(input)?;
    let (input, quantization_table_id) = be_u8(input)?;
    Ok((input, FrameComponent { id, sampling_factors, quantization_table_id }))
}

fn parse_scan_header(input: &[u8]) -> IResult<&[u8], ScanHeader> {
    let (input, marker) = parse_jpeg_marker(input)?;
    let (input, length) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let (input, components) = count(parse_scan_component, component_count as usize)(input)?;
    let (input, spectral_start) = be_u8(input)?;
    let (input, spectral_end) = be_u8(input)?;
    let (input, approximation) = be_u8(input)?;
    Ok((input, ScanHeader {
        marker,
        length,
        component_count,
        components,
        spectral_start,
        spectral_end,
        approximation,
    }))
}

fn parse_scan_component(input: &[u8]) -> IResult<&[u8], ScanComponent> {
    let (input, id) = be_u8(input)?;
    let (input, huffman_tables) = be_u8(input)?;
    Ok((input, ScanComponent { id, huffman_tables }))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegHeader> {
    let (input, soi) = preceded(tag(&[0xFF, 0xD8]), parse_jpeg_marker)(input)?;
    let (input, app_markers) = many0(parse_app_marker)(input)?;
    let (input, quantization_tables) = many0(parse_quantization_table)(input)?;
    let (input, huffman_tables) = many0(parse_huffman_table)(input)?;
    let (input, frame_header) = parse_frame_header(input)?;
    let (input, scan_headers) = many0(parse_scan_header)(input)?;
    let (input, compressed_data) = take_while(|_| true)(input)?;
    let (input, eoi) = preceded(tag(&[0xFF, 0xD9]), parse_jpeg_marker)(input)?;
    
    Ok((input, JpegHeader {
        soi,
        app_markers,
        quantization_tables,
        huffman_tables,
        frame_header,
        scan_headers,
        compressed_data: compressed_data.to_vec(),
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
            println!("Parsed JPEG successfully: {:?}", jpeg);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing JPEG: {}", e);
            std::process::exit(1);
        }
    }
}