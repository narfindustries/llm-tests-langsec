use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    identifier: u8,
    values: Vec<u8>,
}

#[derive(Debug)]
struct HuffmanTable {
    class: u8,
    identifier: u8,
    lengths: Vec<u8>,
    values: Vec<u8>,
}

#[derive(Debug)]
struct FrameComponent {
    identifier: u8,
    sampling_factors: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct StartOfFrame {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<FrameComponent>,
}

#[derive(Debug)]
struct ScanComponent {
    identifier: u8,
    huffman_table: u8,
}

#[derive(Debug)]
struct StartOfScan {
    components: Vec<ScanComponent>,
    spectral_start: u8,
    spectral_end: u8,
    approximation: u8,
}

#[derive(Debug)]
enum Segment {
    SOI,
    EOI,
    DQT(QuantizationTable),
    DHT(HuffmanTable),
    SOF(StartOfFrame),
    SOS(StartOfScan),
    APP(Vec<u8>),
    COM(Vec<u8>),
}

fn parse_quantization_table(input: &[u8]) -> IResult<&[u8], QuantizationTable> {
    let (input, (precision, identifier)) = tuple((be_u8, be_u8))(input)?;
    let (input, values) = take(64usize)(input)?;
    Ok((input, QuantizationTable { precision, identifier, values: values.to_vec() }))
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, (class, identifier, lengths)) = tuple((be_u8, be_u8, take(16usize)))(input)?;
    let total_values = lengths.iter().sum::<u8>() as usize;
    let (input, values) = take(total_values)(input)?;
    Ok((input, HuffmanTable { class, identifier, lengths: lengths.to_vec(), values: values.to_vec() }))
}

fn parse_frame_component(input: &[u8]) -> IResult<&[u8], FrameComponent> {
    let (input, (identifier, sampling_factors, quantization_table_id)) = tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((input, FrameComponent { identifier, sampling_factors, quantization_table_id }))
}

fn parse_start_of_frame(input: &[u8]) -> IResult<&[u8], StartOfFrame> {
    let (input, (precision, height, width, num_components)) = tuple((be_u8, be_u16, be_u16, be_u8))(input)?;
    let (input, components) = take(num_components as usize)(input)?;
    let components = components.iter().map(|&b| parse_frame_component(&[b]).unwrap().1).collect();
    Ok((input, StartOfFrame { precision, height, width, components }))
}

fn parse_scan_component(input: &[u8]) -> IResult<&[u8], ScanComponent> {
    let (input, (identifier, huffman_table)) = tuple((be_u8, be_u8))(input)?;
    Ok((input, ScanComponent { identifier, huffman_table }))
}

fn parse_start_of_scan(input: &[u8]) -> IResult<&[u8], StartOfScan> {
    let (input, num_components) = be_u8(input)?;
    let (input, components) = take(num_components as usize)(input)?;
    let components = components.iter().map(|&b| parse_scan_component(&[b]).unwrap().1).collect();
    let (input, (spectral_start, spectral_end, approximation)) = tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((input, StartOfScan { components, spectral_start, spectral_end, approximation }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = be_u16(input)?;
    match marker {
        0xFFD8 => Ok((input, Segment::SOI)),
        0xFFD9 => Ok((input, Segment::EOI)),
        0xFFDB => parse_quantization_table(input).map(|(i, qt)| (i, Segment::DQT(qt))),
        0xFFC4 => parse_huffman_table(input).map(|(i, ht)| (i, Segment::DHT(ht))),
        0xFFC0..=0xFFC3 | 0xFFC5..=0xFFC7 | 0xFFC9..=0xFFCB => parse_start_of_frame(input).map(|(i, sof)| (i, Segment::SOF(sof))),
        0xFFDA => parse_start_of_scan(input).map(|(i, sos)| (i, Segment::SOS(sos))),
        0xFFE0..=0xFFEF => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::APP(data.to_vec())))
        }
        0xFFFE => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::COM(data.to_vec())))
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Vec<Segment>> {
    let mut segments = Vec::new();
    let mut remaining = input;
    while !remaining.is_empty() {
        let result = parse_segment(remaining)?;
        remaining = result.0;
        segments.push(result.1);
    }
    Ok((remaining, segments))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <JPEG_FILE>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_jpeg(&buffer) {
        Ok((_, segments)) => {
            for segment in segments {
                println!("{:?}", segment);
            }
        }
        Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
    }
}