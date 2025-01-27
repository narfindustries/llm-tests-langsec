use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct JpegMarker {
    marker_type: u8,
    length: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct JpegFrame {
    precision: u8,
    height: u16,
    width: u16,
    num_components: u8,
    components: Vec<FrameComponent>,
}

#[derive(Debug)]
struct FrameComponent {
    id: u8,
    sampling_factor: u8,
    quantization_table: u8,
}

#[derive(Debug)]
struct JpegScan {
    num_components: u8,
    components: Vec<ScanComponent>,
    start_spectral: u8,
    end_spectral: u8,
    approx_high: u8,
    approx_low: u8,
}

#[derive(Debug)]
struct ScanComponent {
    id: u8,
    huffman_table: u8,
}

fn parse_soi(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0xFF, 0xD8])(input)?;
    Ok((input, ()))
}

fn parse_eoi(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(&[0xFF, 0xD9])(input)?;
    Ok((input, ()))
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    let (input, _) = tag(&[0xFF])(input)?;
    let (input, marker_type) = be_u8(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length as usize - 2)(input)?;
    
    Ok((input, JpegMarker {
        marker_type,
        length,
        data: data.to_vec(),
    }))
}

fn parse_frame_header(input: &[u8]) -> IResult<&[u8], JpegFrame> {
    let (input, (precision, height, width, num_components)) = 
        tuple((be_u8, be_u16, be_u16, be_u8))(input)?;
    
    let mut components = Vec::new();
    let mut remaining = input;
    
    for _ in 0..num_components {
        let (input, (id, sampling_factor, quantization_table)) = 
            tuple((be_u8, be_u8, be_u8))(remaining)?;
        
        components.push(FrameComponent {
            id,
            sampling_factor,
            quantization_table,
        });
        
        remaining = input;
    }
    
    Ok((remaining, JpegFrame {
        precision,
        height,
        width,
        num_components,
        components,
    }))
}

fn parse_scan_header(input: &[u8]) -> IResult<&[u8], JpegScan> {
    let (input, num_components) = be_u8(input)?;
    
    let mut components = Vec::new();
    let mut remaining = input;
    
    for _ in 0..num_components {
        let (input, (id, huffman_table)) = tuple((be_u8, be_u8))(remaining)?;
        
        components.push(ScanComponent {
            id,
            huffman_table,
        });
        
        remaining = input;
    }
    
    let (remaining, (start_spectral, end_spectral, approx)) = 
        tuple((be_u8, be_u8, be_u8))(remaining)?;
    
    Ok((remaining, JpegScan {
        num_components,
        components,
        start_spectral,
        end_spectral,
        approx_high: (approx >> 4) & 0x0F,
        approx_low: approx & 0x0F,
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
    
    match parse_soi(input) {
        Ok((remaining, _)) => {
            input = remaining;
            println!("Found SOI marker");
            
            while input.len() > 0 {
                if input[0] == 0xFF {
                    match input[1] {
                        0xD9 => {
                            println!("Found EOI marker");
                            break;
                        },
                        _ => {
                            match parse_marker(input) {
                                Ok((remaining, marker)) => {
                                    println!("Found marker: {:02X}", marker.marker_type);
                                    input = remaining;
                                    
                                    // Parse specific markers
                                    match marker.marker_type {
                                        0xC0..=0xCF => {
                                            if let Ok((_, frame)) = parse_frame_header(&marker.data) {
                                                println!("Frame header: {:?}", frame);
                                            }
                                        },
                                        0xDA => {
                                            if let Ok((_, scan)) = parse_scan_header(&marker.data) {
                                                println!("Scan header: {:?}", scan);
                                            }
                                        },
                                        _ => {}
                                    }
                                },
                                Err(e) => {
                                    eprintln!("Error parsing marker: {:?}", e);
                                    break;
                                }
                            }
                        }
                    }
                } else {
                    input = &input[1..];
                }
            }
        },
        Err(e) => eprintln!("Error parsing SOI: {:?}", e),
    }
}