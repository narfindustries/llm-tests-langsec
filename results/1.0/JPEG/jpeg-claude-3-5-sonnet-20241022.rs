use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u8},
    IResult,
    multi::count,
    sequence::tuple,
};
use std::env;
use std::fs;

#[derive(Debug, Clone)]
struct JpegImage {
    segments: Vec<Segment>,
}

#[derive(Debug, Clone)]
enum Segment {
    StartOfImage,
    EndOfImage,
    StartOfFrame0(FrameHeader),
    StartOfFrame1(FrameHeader),
    StartOfFrame2(FrameHeader),
    StartOfFrame3(FrameHeader),
    StartOfScan(ScanHeader),
    DefineQuantizationTable(QuantizationTable),
    DefineHuffmanTable(HuffmanTable),
    DefineRestartInterval(u16),
    Application(u8, Vec<u8>),
    Comment(Vec<u8>),
}

#[derive(Debug, Clone)]
struct FrameHeader {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<FrameComponent>,
}

#[derive(Debug, Clone)]
struct FrameComponent {
    id: u8,
    h_sampling: u8,
    v_sampling: u8,
    quant_table_num: u8,
}

#[derive(Debug, Clone)]
struct ScanHeader {
    components: Vec<ScanComponent>,
    start_spectral: u8,
    end_spectral: u8,
    approx: u8,
}

#[derive(Debug, Clone)]
struct ScanComponent {
    id: u8,
    dc_ac_tables: u8,
}

#[derive(Debug, Clone)]
struct QuantizationTable {
    precision: u8,
    id: u8,
    values: Vec<u8>,
}

#[derive(Debug, Clone)]
struct HuffmanTable {
    class: u8,
    id: u8,
    counts: Vec<u8>,
    values: Vec<u8>,
}

fn parse_frame_component(input: &[u8]) -> IResult<&[u8], FrameComponent> {
    let (input, (id, sampling_factors, quant_table_num)) = tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((input, FrameComponent {
        id,
        h_sampling: sampling_factors >> 4,
        v_sampling: sampling_factors & 0x0F,
        quant_table_num,
    }))
}

fn parse_frame_header(input: &[u8]) -> IResult<&[u8], FrameHeader> {
    let (input, _length) = be_u16(input)?;
    let (input, (precision, height, width, num_components)) = 
        tuple((be_u8, be_u16, be_u16, be_u8))(input)?;
    let (input, components) = count(parse_frame_component, num_components as usize)(input)?;
    
    Ok((input, FrameHeader {
        precision,
        height,
        width,
        components,
    }))
}

fn parse_scan_component(input: &[u8]) -> IResult<&[u8], ScanComponent> {
    let (input, (id, tables)) = tuple((be_u8, be_u8))(input)?;
    Ok((input, ScanComponent {
        id,
        dc_ac_tables: tables,
    }))
}

fn parse_scan_header(input: &[u8]) -> IResult<&[u8], ScanHeader> {
    let (input, _length) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, components) = count(parse_scan_component, num_components as usize)(input)?;
    let (input, (start_spectral, end_spectral, approx)) = 
        tuple((be_u8, be_u8, be_u8))(input)?;
    
    Ok((input, ScanHeader {
        components,
        start_spectral,
        end_spectral,
        approx,
    }))
}

fn parse_quantization_table(input: &[u8]) -> IResult<&[u8], QuantizationTable> {
    let (input, _length) = be_u16(input)?;
    let (input, info) = be_u8(input)?;
    let precision = info >> 4;
    let id = info & 0x0F;
    let table_size = if precision == 0 { 64 } else { 128 };
    let (input, values) = count(be_u8, table_size)(input)?;
    
    Ok((input, QuantizationTable {
        precision,
        id,
        values,
    }))
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, _length) = be_u16(input)?;
    let (input, info) = be_u8(input)?;
    let class = info >> 4;
    let id = info & 0x0F;
    let (input, counts) = count(be_u8, 16)(input)?;
    let num_values: usize = counts.iter().map(|&x| x as usize).sum();
    let (input, values) = count(be_u8, num_values)(input)?;
    
    Ok((input, HuffmanTable {
        class,
        id,
        counts,
        values,
    }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag(&[0xFF])(input)?;
    let (input, marker) = be_u8(input)?;
    
    match marker {
        0xD8 => Ok((input, Segment::StartOfImage)),
        0xD9 => Ok((input, Segment::EndOfImage)),
        0xC0 => {
            let (input, header) = parse_frame_header(input)?;
            Ok((input, Segment::StartOfFrame0(header)))
        },
        0xC1 => {
            let (input, header) = parse_frame_header(input)?;
            Ok((input, Segment::StartOfFrame1(header)))
        },
        0xC2 => {
            let (input, header) = parse_frame_header(input)?;
            Ok((input, Segment::StartOfFrame2(header)))
        },
        0xC3 => {
            let (input, header) = parse_frame_header(input)?;
            Ok((input, Segment::StartOfFrame3(header)))
        },
        0xDA => {
            let (input, header) = parse_scan_header(input)?;
            Ok((input, Segment::StartOfScan(header)))
        },
        0xDB => {
            let (input, table) = parse_quantization_table(input)?;
            Ok((input, Segment::DefineQuantizationTable(table)))
        },
        0xC4 => {
            let (input, table) = parse_huffman_table(input)?;
            Ok((input, Segment::DefineHuffmanTable(table)))
        },
        0xDD => {
            let (input, _length) = be_u16(input)?;
            let (input, interval) = be_u16(input)?;
            Ok((input, Segment::DefineRestartInterval(interval)))
        },
        0xE0..=0xEF => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length as usize - 2)(input)?;
            Ok((input, Segment::Application(marker - 0xE0, data.to_vec())))
        },
        0xFE => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length as usize - 2)(input)?;
            Ok((input, Segment::Comment(data.to_vec())))
        },
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag
        ))),
    }
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegImage> {
    let mut segments = Vec::new();
    let mut current_input = input;
    
    loop {
        match parse_segment(current_input) {
            Ok((remaining, segment)) => {
                let is_end = matches!(segment, Segment::EndOfImage);
                segments.push(segment);
                if is_end {
                    break;
                }
                current_input = remaining;
            },
            Err(e) => return Err(e),
        }
    }
    
    Ok((current_input, JpegImage { segments }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <jpeg_file>", args[0]);
        std::process::exit(1);
    }

    let data = fs::read(&args[1]).expect("Failed to read file");
    match parse_jpeg(&data) {
        Ok((_, jpeg)) => println!("{:#?}", jpeg),
        Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
    }
}