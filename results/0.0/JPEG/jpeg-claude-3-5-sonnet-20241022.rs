use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::many0,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct JpegImage {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    SOI,
    EOI,
    SOF0(FrameHeader),
    SOF1(FrameHeader),
    SOF2(FrameHeader),
    DHT(HuffmanTable),
    DQT(QuantizationTable),
    SOS(StartOfScan),
    APP0(App0Data),
    APP1(App1Data),
    DRI(RestartInterval),
    COM(CommentData),
    ImageData(Vec<u8>),
}

#[derive(Debug)]
struct FrameHeader {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<FrameComponent>,
}

#[derive(Debug)]
struct FrameComponent {
    id: u8,
    sampling_factors: u8,
    quantization_table: u8,
}

#[derive(Debug)]
struct HuffmanTable {
    class: u8,
    destination: u8,
    lengths: Vec<u8>,
    values: Vec<u8>,
}

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    table_id: u8,
    values: Vec<u8>,
}

#[derive(Debug)]
struct StartOfScan {
    components: Vec<ScanComponent>,
    spectral_start: u8,
    spectral_end: u8,
    approx: u8,
}

#[derive(Debug)]
struct ScanComponent {
    id: u8,
    huffman_table: u8,
}

#[derive(Debug)]
struct App0Data {
    identifier: String,
    version: u16,
    units: u8,
    x_density: u16,
    y_density: u16,
    thumb_width: u8,
    thumb_height: u8,
    thumb_data: Vec<u8>,
}

#[derive(Debug)]
struct App1Data {
    identifier: String,
    data: Vec<u8>,
}

#[derive(Debug)]
struct RestartInterval {
    interval: u16,
}

#[derive(Debug)]
struct CommentData {
    data: Vec<u8>,
}

fn parse_soi(input: &[u8]) -> IResult<&[u8], Segment> {
    map(tag(&[0xFF, 0xD8]), |_| Segment::SOI)(input)
}

fn parse_eoi(input: &[u8]) -> IResult<&[u8], Segment> {
    map(tag(&[0xFF, 0xD9]), |_| Segment::EOI)(input)
}

fn parse_frame_header(input: &[u8]) -> IResult<&[u8], FrameHeader> {
    let (input, _length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    
    let mut components = Vec::new();
    let mut remaining = input;
    
    for _ in 0..component_count {
        let (input, id) = be_u8(remaining)?;
        let (input, sampling_factors) = be_u8(input)?;
        let (input, quantization_table) = be_u8(input)?;
        components.push(FrameComponent {
            id,
            sampling_factors,
            quantization_table,
        });
        remaining = input;
    }

    Ok((remaining, FrameHeader {
        precision,
        height,
        width,
        components,
    }))
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, _length) = be_u16(input)?;
    let (input, info) = be_u8(input)?;
    let class = info >> 4;
    let destination = info & 0x0F;
    
    let (input, lengths) = take(16usize)(input)?;
    let lengths = lengths.to_vec();
    
    let total_values: usize = lengths.iter().map(|&x| x as usize).sum();
    let (input, values) = take(total_values)(input)?;
    
    Ok((input, HuffmanTable {
        class,
        destination,
        lengths,
        values: values.to_vec(),
    }))
}

fn parse_quantization_table(input: &[u8]) -> IResult<&[u8], QuantizationTable> {
    let (input, _length) = be_u16(input)?;
    let (input, info) = be_u8(input)?;
    let precision = info >> 4;
    let table_id = info & 0x0F;
    
    let table_size: usize = if precision == 0 { 64 } else { 128 };
    let (input, values) = take(table_size)(input)?;
    
    Ok((input, QuantizationTable {
        precision,
        table_id,
        values: values.to_vec(),
    }))
}

fn parse_start_of_scan(input: &[u8]) -> IResult<&[u8], StartOfScan> {
    let (input, _length) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    
    let mut components = Vec::new();
    let mut remaining = input;
    
    for _ in 0..component_count {
        let (input, id) = be_u8(remaining)?;
        let (input, huffman_table) = be_u8(input)?;
        components.push(ScanComponent {
            id,
            huffman_table,
        });
        remaining = input;
    }
    
    let (input, spectral_start) = be_u8(remaining)?;
    let (input, spectral_end) = be_u8(input)?;
    let (input, approx) = be_u8(input)?;
    
    Ok((input, StartOfScan {
        components,
        spectral_start,
        spectral_end,
        approx,
    }))
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], App0Data> {
    let (input, _length) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;
    let identifier = String::from_utf8_lossy(identifier).to_string();
    
    let (input, version) = be_u16(input)?;
    let (input, units) = be_u8(input)?;
    let (input, x_density) = be_u16(input)?;
    let (input, y_density) = be_u16(input)?;
    let (input, thumb_width) = be_u8(input)?;
    let (input, thumb_height) = be_u8(input)?;
    
    let thumb_size = (thumb_width as usize) * (thumb_height as usize) * 3;
    let (input, thumb_data) = take(thumb_size)(input)?;
    
    Ok((input, App0Data {
        identifier,
        version,
        units,
        x_density,
        y_density,
        thumb_width,
        thumb_height,
        thumb_data: thumb_data.to_vec(),
    }))
}

fn parse_app1(input: &[u8]) -> IResult<&[u8], App1Data> {
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(6usize)(input)?;
    let identifier = String::from_utf8_lossy(identifier).to_string();
    
    let data_length = length as usize - 8;
    let (input, data) = take(data_length)(input)?;
    
    Ok((input, App1Data {
        identifier,
        data: data.to_vec(),
    }))
}

fn parse_dri(input: &[u8]) -> IResult<&[u8], RestartInterval> {
    let (input, _length) = be_u16(input)?;
    let (input, interval) = be_u16(input)?;
    
    Ok((input, RestartInterval { interval }))
}

fn parse_comment(input: &[u8]) -> IResult<&[u8], CommentData> {
    let (input, length) = be_u16(input)?;
    let data_length = length as usize - 2;
    let (input, data) = take(data_length)(input)?;
    
    Ok((input, CommentData {
        data: data.to_vec(),
    }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = verify(be_u16, |&x| x >= 0xFF00)(input)?;
    
    match marker {
        0xFFD8 => parse_soi(input),
        0xFFD9 => parse_eoi(input),
        0xFFC0 => map(parse_frame_header, Segment::SOF0)(input),
        0xFFC1 => map(parse_frame_header, Segment::SOF1)(input),
        0xFFC2 => map(parse_frame_header, Segment::SOF2)(input),
        0xFFC4 => map(parse_huffman_table, Segment::DHT)(input),
        0xFFDB => map(parse_quantization_table, Segment::DQT)(input),
        0xFFDA => map(parse_start_of_scan, Segment::SOS)(input),
        0xFFE0 => map(parse_app0, Segment::APP0)(input),
        0xFFE1 => map(parse_app1, Segment::APP1)(input),
        0xFFDD => map(parse_dri, Segment::DRI)(input),
        0xFFFE => map(parse_comment, Segment::COM)(input),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegImage> {
    let (input, segments) = many0(parse_segment)(input)?;
    Ok((input, JpegImage { segments }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <jpeg_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_jpeg(&buffer) {
        Ok((_, jpeg)) => println!("{:#?}", jpeg),
        Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
    }
}