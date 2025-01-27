use std::{env, fs::File, io::Read};

use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u8},
    IResult,
};

#[derive(Debug)]
struct JPEG {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    SOI,                  // Start of Image
    APP(Vec<AppData>),    // Application specific
    DQT(Vec<QuantizationTable>), // Define Quantization Table
    SOF0(FrameHeader),    // Start of Frame (Baseline DCT)
    DHT(Vec<HuffmanTable>), // Define Huffman Table
    SOS(ScanHeader),      // Start of Scan
    EOI,                  // End of Image
    COM(String),          // Comment
    Unknown(Vec<u8>),     // Unknown or unprocessed segment
}

#[derive(Debug)]
struct AppData {
    marker: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    identifier: u8,
    values: Vec<u8>,
}

#[derive(Debug)]
struct FrameHeader {
    precision: u8,
    height: u16,
    width: u16,
    num_components: u8,
    components: Vec<FrameComponent>,
}

#[derive(Debug)]
struct FrameComponent {
    identifier: u8,
    sampling_factor: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct HuffmanTable {
    class_and_id: u8,
    codes: Vec<u8>,
}

#[derive(Debug)]
struct ScanHeader {
    num_components: u8,
    components: Vec<ScanComponent>,
    start_of_selection: u8,
    end_of_selection: u8,
    successive_approximation: u8,
}

#[derive(Debug)]
struct ScanComponent {
    identifier: u8,
    huffman_table: u8,
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JPEG> {
    let (input, _) = tag(&[0xFF, 0xD8])(input)?; // SOI marker
    let mut segments = vec![Segment::SOI];

    let mut remaining = input;
    while !remaining.is_empty() {
        let (input, marker) = be_u16(remaining)?;
        remaining = input;
        match marker {
            0xFFD9 => {
                segments.push(Segment::EOI);
                break;
            },
            0xFFE0..=0xFFEF => {
                let (input, app_data) = parse_app_data(remaining)?;
                segments.push(Segment::APP(app_data));
                remaining = input;
            },
            0xFFDB => {
                let (input, dqt) = parse_dqt(remaining)?;
                segments.push(Segment::DQT(dqt));
                remaining = input;
            },
            0xFFC0 => {
                let (input, sof0) = parse_sof0(remaining)?;
                segments.push(Segment::SOF0(sof0));
                remaining = input;
            },
            0xFFC4 => {
                let (input, dht) = parse_dht(remaining)?;
                segments.push(Segment::DHT(dht));
                remaining = input;
            },
            0xFFDA => {
                let (input, sos) = parse_sos(remaining)?;
                segments.push(Segment::SOS(sos));
                remaining = input;
            },
            0xFFFE => {
                let (input, com) = parse_com(remaining)?;
                segments.push(Segment::COM(com));
                remaining = input;
            },
            _ => {
                let (input, data) = parse_unknown_segment(remaining)?;
                segments.push(Segment::Unknown(data));
                remaining = input;
            },
        }
    }

    Ok((remaining, JPEG { segments }))
}

fn parse_app_data(input: &[u8]) -> IResult<&[u8], Vec<AppData>> {
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length - 2)(input)?;
    Ok((input, vec![AppData { marker: length, data: data.to_vec() }]))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], Vec<QuantizationTable>> {
    let (input, length) = be_u16(input)?;
    let end = length as usize - 2;
    let mut remaining = &input[..end];
    let mut tables = Vec::new();

    while !remaining.is_empty() {
        let (input, (precision, identifier)) = nom::sequence::tuple((be_u8, be_u8))(remaining)?;
        let size = if precision & 0xF0 == 0x00 { 64 } else { 128 };
        let (input, values) = take(size)(input)?;
        tables.push(QuantizationTable {
            precision: precision >> 4,
            identifier,
            values: values.to_vec(),
        });
        remaining = input;
    }

    Ok((&input[end..], tables))
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], FrameHeader> {
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (mut remaining, components) = nom::multi::count(parse_frame_component, num_components as usize)(input)?;

    Ok((remaining, FrameHeader {
        precision,
        height,
        width,
        num_components,
        components,
    }))
}

fn parse_frame_component(input: &[u8]) -> IResult<&[u8], FrameComponent> {
    let (input, (identifier, sampling_factor, quantization_table_id)) = nom::sequence::tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((input, FrameComponent {
        identifier,
        sampling_factor,
        quantization_table_id,
    }))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], Vec<HuffmanTable>> {
    let (input, length) = be_u16(input)?;
    let end = length as usize - 2;
    let mut remaining = &input[..end];
    let mut tables = Vec::new();

    while remaining.len() > 16 {
        let (input, class_and_id) = be_u8(remaining)?;
        let (input, code_lengths) = take(16usize)(input)?;
        let num_codes = code_lengths.iter().map(|&x| x as usize).sum();
        let (input, codes) = take(num_codes)(input)?;
        tables.push(HuffmanTable {
            class_and_id,
            codes: codes.to_vec(),
        });
        remaining = input;
    }

    Ok((&input[end..], tables))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], ScanHeader> {
    let (input, length) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, components) = nom::multi::count(parse_scan_component, num_components as usize)(input)?;
    let (input, (start_of_selection, end_of_selection, successive_approximation)) = nom::sequence::tuple((be_u8, be_u8, be_u8))(input)?;

    Ok((input, ScanHeader {
        num_components,
        components,
        start_of_selection,
        end_of_selection,
        successive_approximation,
    }))
}

fn parse_scan_component(input: &[u8]) -> IResult<&[u8], ScanComponent> {
    let (input, (identifier, huffman_table)) = nom::sequence::tuple((be_u8, be_u8))(input)?;
    Ok((input, ScanComponent {
        identifier,
        huffman_table,
    }))
}

fn parse_com(input: &[u8]) -> IResult<&[u8], String> {
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length - 2)(input)?;
    match std::str::from_utf8(data) {
        Ok(v) => Ok((input, v.to_string())),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Fail))),
    }
}

fn parse_unknown_segment(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length - 2)(input)?;
    Ok((input, data.to_vec()))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <JPEG_FILE>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("File not found");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_jpeg(&buffer) {
        Ok((_, jpeg)) => println!("{:?}", jpeg),
        Err(e) => println!("Error parsing JPEG: {:?}", e),
    }
}