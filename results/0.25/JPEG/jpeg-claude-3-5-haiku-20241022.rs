use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{many0, many1},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct JpegFile {
    start_of_image: u16,
    segments: Vec<JpegSegment>,
}

#[derive(Debug)]
enum JpegSegment {
    APP0 {
        length: u16,
        identifier: Vec<u8>,
        version: (u8, u8),
        density_units: u8,
        x_density: u16,
        y_density: u16,
        thumbnail_x: u8,
        thumbnail_y: u8,
        thumbnail_data: Vec<u8>,
    },
    SOFn {
        precision: u8,
        height: u16,
        width: u16,
        components: Vec<SOFComponent>,
    },
    DHT {
        class: u8,
        destination_id: u8,
        huffman_table: Vec<u8>,
    },
    DQT {
        precision: u8,
        table_id: u8,
        quantization_table: Vec<u8>,
    },
    SOS {
        num_components: u8,
        component_specs: Vec<SOSComponent>,
        spectral_selection_start: u8,
        spectral_selection_end: u8,
        successive_approximation: u8,
    },
    EOI,
}

#[derive(Debug)]
struct SOFComponent {
    id: u8,
    sampling_factors: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct SOSComponent {
    id: u8,
    dc_huffman_table_id: u8,
    ac_huffman_table_id: u8,
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegFile> {
    let (input, start_of_image) = be_u16(input)?;
    let (input, segments) = many1(parse_segment)(input)?;
    Ok((input, JpegFile { start_of_image, segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, marker) = be_u16(input)?;
    match marker {
        0xFFE0 => parse_app0(input),
        0xFFC0 | 0xFFC1 | 0xFFC2 | 0xFFC3 => parse_sof(input, marker),
        0xFFC4 => parse_dht(input),
        0xFFDB => parse_dqt(input),
        0xFFDA => parse_sos(input),
        0xFFD9 => Ok((input, JpegSegment::EOI)),
        _ => take(0usize)(input).map(|(i, _)| (i, JpegSegment::EOI)),
    }
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;
    let (input, version) = tuple((be_u8, be_u8))(input)?;
    let (input, density_units) = be_u8(input)?;
    let (input, x_density) = be_u16(input)?;
    let (input, y_density) = be_u16(input)?;
    let (input, thumbnail_x) = be_u8(input)?;
    let (input, thumbnail_y) = be_u8(input)?;
    let (input, thumbnail_data) = take(thumbnail_x as usize * thumbnail_y as usize * 3)(input)?;

    Ok((input, JpegSegment::APP0 {
        length,
        identifier: identifier.to_vec(),
        version,
        density_units,
        x_density,
        y_density,
        thumbnail_x,
        thumbnail_y,
        thumbnail_data: thumbnail_data.to_vec(),
    }))
}

fn parse_sof(input: &[u8], marker: u16) -> IResult<&[u8], JpegSegment> {
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, components) = many1(parse_sof_component)(input)?;

    Ok((input, JpegSegment::SOFn {
        precision,
        height,
        width,
        components,
    }))
}

fn parse_sof_component(input: &[u8]) -> IResult<&[u8], SOFComponent> {
    let (input, id) = be_u8(input)?;
    let (input, sampling_factors) = be_u8(input)?;
    let (input, quantization_table_id) = be_u8(input)?;

    Ok((input, SOFComponent {
        id,
        sampling_factors,
        quantization_table_id,
    }))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, length) = be_u16(input)?;
    let (input, class) = be_u8(input)?;
    let (input, destination_id) = be_u8(input)?;
    let (input, huffman_table) = take(length as usize - 3)(input)?;

    Ok((input, JpegSegment::DHT {
        class,
        destination_id,
        huffman_table: huffman_table.to_vec(),
    }))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, table_id) = be_u8(input)?;
    let (input, quantization_table) = take(length as usize - 3)(input)?;

    Ok((input, JpegSegment::DQT {
        precision,
        table_id,
        quantization_table: quantization_table.to_vec(),
    }))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, length) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, component_specs) = many1(parse_sos_component)(input)?;
    let (input, spectral_selection_start) = be_u8(input)?;
    let (input, spectral_selection_end) = be_u8(input)?;
    let (input, successive_approximation) = be_u8(input)?;

    Ok((input, JpegSegment::SOS {
        num_components,
        component_specs,
        spectral_selection_start,
        spectral_selection_end,
        successive_approximation,
    }))
}

fn parse_sos_component(input: &[u8]) -> IResult<&[u8], SOSComponent> {
    let (input, id) = be_u8(input)?;
    let (input, dc_huffman_table_id) = be_u8(input)?;
    let (input, ac_huffman_table_id) = be_u8(input)?;

    Ok((input, SOSComponent {
        id,
        dc_huffman_table_id,
        ac_huffman_table_id,
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
            println!("Parsed JPEG: {:?}", jpeg);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing JPEG: {:?}", e);
            std::process::exit(1);
        }
    }
}