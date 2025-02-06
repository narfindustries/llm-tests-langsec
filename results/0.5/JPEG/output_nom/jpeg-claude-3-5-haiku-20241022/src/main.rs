use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16},
    multi::{many0, count},
    sequence::{tuple, preceded},
    branch::alt,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct JpegMarker(u16);

#[derive(Debug)]
struct JfifHeader {
    version: u16,
    density_units: u8,
    x_density: u16,
    y_density: u16,
}

#[derive(Debug)]
struct SOFSegment {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<Component>,
}

#[derive(Debug)]
struct Component {
    id: u8,
    sampling_factor: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct JpegFile {
    markers: Vec<JpegMarker>,
    jfif_header: Option<JfifHeader>,
    sof_segment: Option<SOFSegment>,
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    let (input, marker) = preceded(tag(&[0xFF]), be_u16)(input)?;
    Ok((input, JpegMarker(marker)))
}

fn parse_jfif_header(input: &[u8]) -> IResult<&[u8], JfifHeader> {
    let (input, _) = tag(b"JFIF\0")(input)?;
    let (input, (version, density_units, x_density, y_density)) = tuple((
        be_u16, be_u8, be_u16, be_u16
    ))(input)?;
    Ok((input, JfifHeader {
        version, density_units, x_density, y_density
    }))
}

fn parse_sof_segment(input: &[u8]) -> IResult<&[u8], SOFSegment> {
    let (input, (precision, height, width, component_count)) = tuple((
        be_u8, be_u16, be_u16, be_u8
    ))(input)?;
    let (input, components) = count(parse_component, component_count as usize)(input)?;
    Ok((input, SOFSegment {
        precision, height, width, components
    }))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, (id, sampling_factor, quantization_table_id)) = tuple((
        be_u8, be_u8, be_u8
    ))(input)?;
    Ok((input, Component {
        id, sampling_factor, quantization_table_id
    }))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegFile> {
    let (input, _) = tag(&[0xFF, 0xD8])(input)?;
    let (input, markers) = many0(parse_marker)(input)?;
    
    let mut jfif_header = None;
    let mut sof_segment = None;

    for marker in &markers {
        match marker.0 {
            0xE0 => {
                // JFIF Header
                let (_, header) = parse_jfif_header(input)?;
                jfif_header = Some(header);
            },
            0xC0 | 0xC1 | 0xC2 => {
                // Start of Frame
                let (_, segment) = parse_sof_segment(input)?;
                sof_segment = Some(segment);
            },
            _ => {}
        }
    }

    Ok((input, JpegFile {
        markers,
        jfif_header,
        sof_segment,
    }))
}

fn main() -> std::io::Result<()> {
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
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}