use nom::bytes::complete::{tag, take};
use nom::number::complete::{be_u16, be_u8};
use nom::IResult;
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
    APP(AppSegment),
    COM(CommentSegment),
    DRI(RestartInterval),
    Data(Vec<u8>),
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
    h_sampling: u8,
    v_sampling: u8,
    qt_number: u8,
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
    dc_table: u8,
    ac_table: u8,
}

#[derive(Debug)]
struct AppSegment {
    marker: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
struct CommentSegment {
    data: Vec<u8>,
}

#[derive(Debug)]
struct RestartInterval {
    interval: u16,
}

fn parse_soi(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag([0xFF, 0xD8])(input)?;
    Ok((input, Segment::SOI))
}

fn parse_eoi(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag([0xFF, 0xD9])(input)?;
    Ok((input, Segment::EOI))
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
        let (input, sampling) = be_u8(input)?;
        let h_sampling = sampling >> 4;
        let v_sampling = sampling & 0x0F;
        let (input, qt_number) = be_u8(input)?;
        
        components.push(FrameComponent {
            id,
            h_sampling,
            v_sampling,
            qt_number,
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
        let (input, tables) = be_u8(input)?;
        let dc_table = tables >> 4;
        let ac_table = tables & 0x0F;
        
        components.push(ScanComponent {
            id,
            dc_table,
            ac_table,
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

fn parse_app_segment(input: &[u8]) -> IResult<&[u8], AppSegment> {
    let (input, marker) = be_u8(input)?;
    let (input, length) = be_u16(input)?;
    let data_length = length as usize - 2;
    let (input, data) = take(data_length)(input)?;
    
    Ok((input, AppSegment {
        marker,
        data: data.to_vec(),
    }))
}

fn parse_comment_segment(input: &[u8]) -> IResult<&[u8], CommentSegment> {
    let (input, length) = be_u16(input)?;
    let data_length = length as usize - 2;
    let (input, data) = take(data_length)(input)?;
    
    Ok((input, CommentSegment {
        data: data.to_vec(),
    }))
}

fn parse_restart_interval(input: &[u8]) -> IResult<&[u8], RestartInterval> {
    let (input, _length) = be_u16(input)?;
    let (input, interval) = be_u16(input)?;
    
    Ok((input, RestartInterval { interval }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag([0xFF])(input)?;
    let (input, segment_type) = be_u8(input)?;
    
    match segment_type {
        0xD8 => Ok((input, Segment::SOI)),
        0xD9 => Ok((input, Segment::EOI)),
        0xC0 => {
            let (input, frame) = parse_frame_header(input)?;
            Ok((input, Segment::SOF0(frame)))
        },
        0xC1 => {
            let (input, frame) = parse_frame_header(input)?;
            Ok((input, Segment::SOF1(frame)))
        },
        0xC2 => {
            let (input, frame) = parse_frame_header(input)?;
            Ok((input, Segment::SOF2(frame)))
        },
        0xC4 => {
            let (input, table) = parse_huffman_table(input)?;
            Ok((input, Segment::DHT(table)))
        },
        0xDB => {
            let (input, table) = parse_quantization_table(input)?;
            Ok((input, Segment::DQT(table)))
        },
        0xDA => {
            let (input, scan) = parse_start_of_scan(input)?;
            Ok((input, Segment::SOS(scan)))
        },
        0xE0..=0xEF => {
            let (input, app) = parse_app_segment(input)?;
            Ok((input, Segment::APP(app)))
        },
        0xFE => {
            let (input, comment) = parse_comment_segment(input)?;
            Ok((input, Segment::COM(comment)))
        },
        0xDD => {
            let (input, dri) = parse_restart_interval(input)?;
            Ok((input, Segment::DRI(dri)))
        },
        _ => {
            let (input, length) = be_u16(input)?;
            let data_length = length as usize - 2;
            let (input, data) = take(data_length)(input)?;
            Ok((input, Segment::Data(data.to_vec())))
        }
    }
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegImage> {
    let mut segments = Vec::new();
    let mut remaining = input;
    
    while !remaining.is_empty() {
        let (input, segment) = parse_segment(remaining)?;
        segments.push(segment);
        remaining = input;
        
        if let Segment::EOI = segments.last().unwrap() {
            break;
        }
    }
    
    Ok((remaining, JpegImage { segments }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <jpeg_file>", args[0]);
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