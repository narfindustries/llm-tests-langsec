use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::many0,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct JpegImage {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    SOI,
    APP0(APP0Segment),
    APP1(Vec<u8>),
    DQT(Vec<u8>),
    SOF0(SOF0Segment),
    DHT(Vec<u8>),
    SOS(SOSSegment),
    COM(Vec<u8>),
    EOI,
    Other { marker: u8, data: Vec<u8> },
}

#[derive(Debug)]
struct APP0Segment {
    identifier: [u8; 5],
    version: (u8, u8),
    density_units: u8,
    x_density: u16,
    y_density: u16,
    thumbnail_width: u8,
    thumbnail_height: u8,
    thumbnail_data: Vec<u8>,
}

#[derive(Debug)]
struct SOF0Segment {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<ComponentInfo>,
}

#[derive(Debug)]
struct ComponentInfo {
    id: u8,
    sampling_factors: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct SOSSegment {
    components: Vec<ScanComponentInfo>,
    start_spectral: u8,
    end_spectral: u8,
    approx_high: u8,
    approx_low: u8,
    image_data: Vec<u8>,
}

#[derive(Debug)]
struct ScanComponentInfo {
    component_id: u8,
    dc_ac_table_ids: u8,
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], APP0Segment> {
    let (input, _) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;
    let (input, version_major) = be_u8(input)?;
    let (input, version_minor) = be_u8(input)?;
    let (input, density_units) = be_u8(input)?;
    let (input, x_density) = be_u16(input)?;
    let (input, y_density) = be_u16(input)?;
    let (input, thumbnail_width) = be_u8(input)?;
    let (input, thumbnail_height) = be_u8(input)?;
    let thumbnail_size = (thumbnail_width as usize) * (thumbnail_height as usize) * 3;
    let (input, thumbnail_data) = take(thumbnail_size)(input)?;

    Ok((
        input,
        APP0Segment {
            identifier: identifier.try_into().unwrap(),
            version: (version_major, version_minor),
            density_units,
            x_density,
            y_density,
            thumbnail_width,
            thumbnail_height,
            thumbnail_data: thumbnail_data.to_vec(),
        },
    ))
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], SOF0Segment> {
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    
    let mut components = Vec::new();
    let mut remaining = input;
    
    for _ in 0..component_count {
        let (input, id) = be_u8(remaining)?;
        let (input, sampling_factors) = be_u8(input)?;
        let (input, quantization_table_id) = be_u8(input)?;
        components.push(ComponentInfo {
            id,
            sampling_factors,
            quantization_table_id,
        });
        remaining = input;
    }

    Ok((
        remaining,
        SOF0Segment {
            precision,
            height,
            width,
            components,
        },
    ))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], SOSSegment> {
    let (input, length) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    
    let mut components = Vec::new();
    let mut remaining = input;
    
    for _ in 0..component_count {
        let (input, component_id) = be_u8(remaining)?;
        let (input, dc_ac_table_ids) = be_u8(input)?;
        components.push(ScanComponentInfo {
            component_id,
            dc_ac_table_ids,
        });
        remaining = input;
    }

    let (input, start_spectral) = be_u8(remaining)?;
    let (input, end_spectral) = be_u8(input)?;
    let (input, approx_bits) = be_u8(input)?;
    let approx_high = (approx_bits >> 4) & 0x0F;
    let approx_low = approx_bits & 0x0F;

    // The rest is image data until EOI marker
    let mut image_data = Vec::new();
    let mut i = 0;
    while i < input.len() {
        if i + 1 < input.len() && input[i] == 0xFF && input[i + 1] == 0xD9 {
            break;
        }
        image_data.push(input[i]);
        i += 1;
    }

    Ok((
        &input[i..],
        SOSSegment {
            components,
            start_spectral,
            end_spectral,
            approx_high,
            approx_low,
            image_data,
        },
    ))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = verify(be_u8, |&x| x == 0xFF)(input)?;
    let (input, segment_type) = be_u8(input)?;

    match segment_type {
        0xD8 => Ok((input, Segment::SOI)),
        0xE0 => map(parse_app0, Segment::APP0)(input),
        0xE1 => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length as usize - 2)(input)?;
            Ok((input, Segment::APP1(data.to_vec())))
        }
        0xDB => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length as usize - 2)(input)?;
            Ok((input, Segment::DQT(data.to_vec())))
        }
        0xC0 => map(parse_sof0, Segment::SOF0)(input),
        0xC4 => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length as usize - 2)(input)?;
            Ok((input, Segment::DHT(data.to_vec())))
        }
        0xDA => map(parse_sos, Segment::SOS)(input),
        0xFE => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length as usize - 2)(input)?;
            Ok((input, Segment::COM(data.to_vec())))
        }
        0xD9 => Ok((input, Segment::EOI)),
        _ => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length as usize - 2)(input)?;
            Ok((
                input,
                Segment::Other {
                    marker: segment_type,
                    data: data.to_vec(),
                },
            ))
        }
    }
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegImage> {
    map(many0(parse_segment), |segments| JpegImage { segments })(input)
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
        Ok((remaining, jpeg)) => {
            println!("Successfully parsed JPEG: {:#?}", jpeg);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining unparsed", remaining.len());
            }
        }
        Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
    }
}