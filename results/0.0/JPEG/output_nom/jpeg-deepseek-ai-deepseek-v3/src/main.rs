use nom::{
    bytes::complete::take,
    number::complete::{be_u8, be_u16},
    sequence::tuple,
    IResult, error::{Error, ErrorKind},
};
use std::env;
use std::fs;

#[derive(Debug)]
struct JPEG {
    soi: Marker,
    segments: Vec<Segment>,
    eoi: Marker,
}

#[derive(Debug)]
enum Segment {
    SOF(SOF),
    DHT(DHT),
    DQT(DQT),
    SOS(SOS),
    APP(APP),
    COM(COM),
}

#[derive(Debug)]
struct Marker(u16);

#[derive(Debug)]
struct SOF {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<Component>,
}

#[derive(Debug)]
struct Component {
    id: u8,
    sampling_factors: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct DHT {
    table_class: u8,
    table_destination_id: u8,
    huffman_codes: Vec<u8>,
}

#[derive(Debug)]
struct DQT {
    table_precision: u8,
    table_destination_id: u8,
    quantization_values: Vec<u8>,
}

#[derive(Debug)]
struct SOS {
    components: Vec<SOSComponent>,
    spectral_selection: (u8, u8),
    successive_approximation: u8,
}

#[derive(Debug)]
struct SOSComponent {
    id: u8,
    dc_table_id: u8,
    ac_table_id: u8,
}

#[derive(Debug)]
struct APP {
    identifier: Vec<u8>,
    data: Vec<u8>,
}

#[derive(Debug)]
struct COM {
    comment: Vec<u8>,
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JPEG> {
    let (input, soi) = parse_soi(input)?;
    let (input, segments) = parse_segments(input)?;
    let (input, eoi) = parse_eoi(input)?;
    Ok((input, JPEG { soi, segments, eoi }))
}

fn parse_soi(input: &[u8]) -> IResult<&[u8], Marker> {
    let (input, marker) = be_u16(input)?;
    if marker == 0xFFD8 {
        Ok((input, Marker(marker)))
    } else {
        Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)))
    }
}

fn parse_eoi(input: &[u8]) -> IResult<&[u8], Marker> {
    let (input, marker) = be_u16(input)?;
    if marker == 0xFFD9 {
        Ok((input, Marker(marker)))
    } else {
        Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)))
    }
}

fn parse_segments(input: &[u8]) -> IResult<&[u8], Vec<Segment>> {
    let mut segments = Vec::new();
    let mut remaining = input;
    while !remaining.is_empty() {
        let (new_remaining, segment) = parse_segment(remaining)?;
        segments.push(segment);
        remaining = new_remaining;
    }
    Ok((remaining, segments))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = be_u16(input)?;
    match marker {
        0xFFC0..=0xFFC3 => {
            let (input, sof) = parse_sof(input)?;
            Ok((input, Segment::SOF(sof)))
        }
        0xFFC4 => {
            let (input, dht) = parse_dht(input)?;
            Ok((input, Segment::DHT(dht)))
        }
        0xFFDB => {
            let (input, dqt) = parse_dqt(input)?;
            Ok((input, Segment::DQT(dqt)))
        }
        0xFFDA => {
            let (input, sos) = parse_sos(input)?;
            Ok((input, Segment::SOS(sos)))
        }
        0xFFE0..=0xFFEF => {
            let (input, app) = parse_app(input)?;
            Ok((input, Segment::APP(app)))
        }
        0xFFFE => {
            let (input, com) = parse_com(input)?;
            Ok((input, Segment::COM(com)))
        }
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn parse_sof(input: &[u8]) -> IResult<&[u8], SOF> {
    let (input, (precision, height, width, num_components)) =
        tuple((be_u8, be_u16, be_u16, be_u8))(input)?;
    let (input, components) = take(num_components as usize * 3)(input)?;
    let components = components
        .chunks(3)
        .map(|chunk| Component {
            id: chunk[0],
            sampling_factors: chunk[1],
            quantization_table_id: chunk[2],
        })
        .collect();
    Ok((input, SOF {
        precision,
        height,
        width,
        components,
    }))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], DHT> {
    let (input, (table_class, table_destination_id)) = tuple((be_u8, be_u8))(input)?;
    let (input, huffman_codes) = take(16usize)(input)?;
    Ok((input, DHT {
        table_class,
        table_destination_id,
        huffman_codes: huffman_codes.to_vec(),
    }))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], DQT> {
    let (input, (table_precision, table_destination_id)) = tuple((be_u8, be_u8))(input)?;
    let (input, quantization_values) = take(64usize)(input)?;
    Ok((input, DQT {
        table_precision,
        table_destination_id,
        quantization_values: quantization_values.to_vec(),
    }))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], SOS> {
    let (input, num_components) = be_u8(input)?;
    let (input, components) = take(num_components as usize * 2)(input)?;
    let components = components
        .chunks(2)
        .map(|chunk| SOSComponent {
            id: chunk[0],
            dc_table_id: chunk[1] >> 4,
            ac_table_id: chunk[1] & 0x0F,
        })
        .collect();
    let (input, (spectral_start, spectral_end, successive_approximation)) =
        tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((input, SOS {
        components,
        spectral_selection: (spectral_start, spectral_end),
        successive_approximation,
    }))
}

fn parse_app(input: &[u8]) -> IResult<&[u8], APP> {
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(4usize)(input)?;
    let (input, data) = take(length - 2 - 4)(input)?;
    Ok((input, APP {
        identifier: identifier.to_vec(),
        data: data.to_vec(),
    }))
}

fn parse_com(input: &[u8]) -> IResult<&[u8], COM> {
    let (input, length) = be_u16(input)?;
    let (input, comment) = take(length - 2)(input)?;
    Ok((input, COM {
        comment: comment.to_vec(),
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }
    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");
    match parse_jpeg(&data) {
        Ok((_, jpeg)) => println!("{:#?}", jpeg),
        Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
    }
}