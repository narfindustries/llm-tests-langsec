use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
    multi::many0,
};

#[derive(Debug)]
struct JFIFHeader {
    identifier: [u8; 5],
    version: u16,
    units: u8,
    x_density: u16,
    y_density: u16,
    thumb_width: u8,
    thumb_height: u8,
    thumb_data: Vec<u8>,
}

#[derive(Debug)]
struct QuantizationTable {
    precision_and_id: u8,
    table: Vec<u8>,
}

#[derive(Debug)]
struct HuffmanTable {
    class_and_id: u8,
    counts: Vec<u8>,
    values: Vec<u8>,
}

#[derive(Debug)]
struct FrameComponent {
    id: u8,
    sampling_factors: u8,
    qt_selector: u8,
}

#[derive(Debug)]
struct Frame {
    precision: u8,
    height: u16,
    width: u16,
    num_components: u8,
    components: Vec<FrameComponent>,
}

#[derive(Debug)]
struct ScanComponent {
    id: u8,
    huffman_selectors: u8,
}

#[derive(Debug)]
struct Scan {
    num_components: u8,
    components: Vec<ScanComponent>,
    start_spectral: u8,
    end_spectral: u8,
    approx_high_low: u8,
    image_data: Vec<u8>,
}

#[derive(Debug)]
struct JPEGSegment {
    marker: u8,
    length: u16,
    data: Vec<u8>,
}

fn parse_jfif_header(input: &[u8]) -> IResult<&[u8], JFIFHeader> {
    let (input, identifier) = take(5usize)(input)?;
    let (input, version) = be_u16(input)?;
    let (input, units) = be_u8(input)?;
    let (input, x_density) = be_u16(input)?;
    let (input, y_density) = be_u16(input)?;
    let (input, thumb_width) = be_u8(input)?;
    let (input, thumb_height) = be_u8(input)?;
    let thumb_size = (thumb_width as usize) * (thumb_height as usize) * 3;
    let (input, thumb_data) = take(thumb_size)(input)?;
    
    Ok((input, JFIFHeader {
        identifier: identifier.try_into().unwrap(),
        version,
        units,
        x_density,
        y_density,
        thumb_width,
        thumb_height,
        thumb_data: thumb_data.to_vec(),
    }))
}

fn parse_quantization_table(input: &[u8]) -> IResult<&[u8], QuantizationTable> {
    let (input, precision_and_id) = be_u8(input)?;
    let precision = (precision_and_id >> 4) & 0x0F;
    let table_size = if precision == 0 { 64 } else { 128 };
    let (input, table) = take(table_size)(input)?;
    
    Ok((input, QuantizationTable {
        precision_and_id,
        table: table.to_vec(),
    }))
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, class_and_id) = be_u8(input)?;
    let (input, counts) = take(16usize)(input)?;
    let num_values: usize = counts.iter().map(|&x| x as usize).sum();
    let (input, values) = take(num_values)(input)?;
    
    Ok((input, HuffmanTable {
        class_and_id,
        counts: counts.to_vec(),
        values: values.to_vec(),
    }))
}

fn parse_frame_component(input: &[u8]) -> IResult<&[u8], FrameComponent> {
    let (input, (id, sampling_factors, qt_selector)) = tuple((be_u8, be_u8, be_u8))(input)?;
    
    Ok((input, FrameComponent {
        id,
        sampling_factors,
        qt_selector,
    }))
}

fn parse_frame(input: &[u8]) -> IResult<&[u8], Frame> {
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let mut components = Vec::new();
    let mut remaining = input;
    
    for _ in 0..num_components {
        let (new_input, component) = parse_frame_component(remaining)?;
        components.push(component);
        remaining = new_input;
    }
    
    Ok((remaining, Frame {
        precision,
        height,
        width,
        num_components,
        components,
    }))
}

fn parse_scan_component(input: &[u8]) -> IResult<&[u8], ScanComponent> {
    let (input, (id, huffman_selectors)) = tuple((be_u8, be_u8))(input)?;
    
    Ok((input, ScanComponent {
        id,
        huffman_selectors,
    }))
}

fn parse_scan(input: &[u8]) -> IResult<&[u8], Scan> {
    let (input, num_components) = be_u8(input)?;
    let mut components = Vec::new();
    let mut remaining = input;
    
    for _ in 0..num_components {
        let (new_input, component) = parse_scan_component(remaining)?;
        components.push(component);
        remaining = new_input;
    }
    
    let (remaining, start_spectral) = be_u8(remaining)?;
    let (remaining, end_spectral) = be_u8(remaining)?;
    let (remaining, approx_high_low) = be_u8(remaining)?;
    
    Ok((remaining, Scan {
        num_components,
        components,
        start_spectral,
        end_spectral,
        approx_high_low,
        image_data: remaining.to_vec(),
    }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], JPEGSegment> {
    let (input, _) = tag(&[0xFF])(input)?;
    let (input, marker) = be_u8(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length as usize - 2)(input)?;
    
    Ok((input, JPEGSegment {
        marker,
        length,
        data: data.to_vec(),
    }))
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

    let mut input = buffer.as_slice();
    let mut segments = Vec::new();

    while !input.is_empty() {
        match parse_segment(input) {
            Ok((remaining, segment)) => {
                segments.push(segment);
                input = remaining;
            }
            Err(e) => {
                eprintln!("Error parsing JPEG: {:?}", e);
                break;
            }
        }
    }

    println!("Found {} segments", segments.len());
    for (i, segment) in segments.iter().enumerate() {
        println!("Segment {}: Marker: 0x{:02X}, Length: {}", i, segment.marker, segment.length);
        
        match segment.marker {
            0xE0 => {
                if let Ok((_, header)) = parse_jfif_header(&segment.data) {
                    println!("JFIF Header: {:?}", header);
                }
            }
            0xDB => {
                if let Ok((_, qt)) = parse_quantization_table(&segment.data) {
                    println!("Quantization Table: {:?}", qt);
                }
            }
            0xC4 => {
                if let Ok((_, ht)) = parse_huffman_table(&segment.data) {
                    println!("Huffman Table: {:?}", ht);
                }
            }
            0xC0 => {
                if let Ok((_, frame)) = parse_frame(&segment.data) {
                    println!("Start of Frame: {:?}", frame);
                }
            }
            0xDA => {
                if let Ok((_, scan)) = parse_scan(&segment.data) {
                    println!("Start of Scan: {:?}", scan);
                }
            }
            _ => {}
        }
    }
}