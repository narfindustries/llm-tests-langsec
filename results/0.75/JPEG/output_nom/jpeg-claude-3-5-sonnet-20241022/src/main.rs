use nom::bytes::complete::{tag, take};
use nom::combinator::map;
use nom::multi::count;
use nom::number::complete::{be_u16, be_u8};
use nom::sequence::tuple;
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct JPEG {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    SOI,
    APP0(APP0),
    APP1(Vec<u8>),
    DQT(DQT),
    SOF0(SOF),
    DHT(DHT),
    SOS(SOS),
    DRI(DRI),
    RST(u8),
    COM(Vec<u8>),
    EOI,
    EncodedData(Vec<u8>),
}

#[derive(Debug)]
struct APP0 {
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
struct DQT {
    precision: u8,
    table_id: u8,
    values: Vec<u8>,
}

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
    h_sampling: u8,
    v_sampling: u8,
    qt_id: u8,
}

#[derive(Debug)]
struct DHT {
    table_type: u8,
    table_id: u8,
    lengths: Vec<u8>,
    values: Vec<u8>,
}

#[derive(Debug)]
struct SOS {
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
struct DRI {
    interval: u16,
}

fn parse_soi(input: &[u8]) -> IResult<&[u8], Segment> {
    map(tag(&[0xFF, 0xD8]), |_| Segment::SOI)(input)
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag(&[0xFF, 0xE0])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;
    let (input, version) = be_u16(input)?;
    let (input, units) = be_u8(input)?;
    let (input, x_density) = be_u16(input)?;
    let (input, y_density) = be_u16(input)?;
    let (input, thumb_width) = be_u8(input)?;
    let (input, thumb_height) = be_u8(input)?;
    let (input, thumb_data) = take((thumb_width as usize) * (thumb_height as usize) * 3)(input)?;
    
    Ok((input, Segment::APP0(APP0 {
        identifier: identifier.try_into().unwrap(),
        version,
        units,
        x_density,
        y_density,
        thumb_width,
        thumb_height,
        thumb_data: thumb_data.to_vec(),
    })))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag(&[0xFF, 0xDB])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, info) = be_u8(input)?;
    let precision = (info >> 4) & 0x0F;
    let table_id = info & 0x0F;
    let (input, values) = take(64usize)(input)?;
    
    Ok((input, Segment::DQT(DQT {
        precision,
        table_id,
        values: values.to_vec(),
    })))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, id) = be_u8(input)?;
    let (input, sampling) = be_u8(input)?;
    let (input, qt_id) = be_u8(input)?;
    
    Ok((input, Component {
        id,
        h_sampling: (sampling >> 4) & 0x0F,
        v_sampling: sampling & 0x0F,
        qt_id,
    }))
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag(&[0xFF, 0xC0])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let (input, components) = count(parse_component, component_count as usize)(input)?;
    
    Ok((input, Segment::SOF0(SOF {
        precision,
        height,
        width,
        components,
    })))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag(&[0xFF, 0xC4])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, info) = be_u8(input)?;
    let table_type = (info >> 4) & 0x0F;
    let table_id = info & 0x0F;
    let (input, lengths) = count(be_u8, 16)(input)?;
    let sum: usize = lengths.iter().map(|&x| x as usize).sum();
    let (input, values) = take(sum)(input)?;
    
    Ok((input, Segment::DHT(DHT {
        table_type,
        table_id,
        lengths,
        values: values.to_vec(),
    })))
}

fn parse_scan_component(input: &[u8]) -> IResult<&[u8], ScanComponent> {
    let (input, id) = be_u8(input)?;
    let (input, tables) = be_u8(input)?;
    
    Ok((input, ScanComponent {
        id,
        dc_table: (tables >> 4) & 0x0F,
        ac_table: tables & 0x0F,
    }))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag(&[0xFF, 0xDA])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let (input, components) = count(parse_scan_component, component_count as usize)(input)?;
    let (input, spectral_start) = be_u8(input)?;
    let (input, spectral_end) = be_u8(input)?;
    let (input, approx) = be_u8(input)?;
    
    Ok((input, Segment::SOS(SOS {
        components,
        spectral_start,
        spectral_end,
        approx,
    })))
}

fn parse_dri(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag(&[0xFF, 0xDD])(input)?;
    let (input, _length) = be_u16(input)?;
    let (input, interval) = be_u16(input)?;
    
    Ok((input, Segment::DRI(DRI { interval })))
}

fn parse_rst(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag(&[0xFF])(input)?;
    let (input, marker) = be_u8(input)?;
    if marker >= 0xD0 && marker <= 0xD7 {
        Ok((input, Segment::RST(marker - 0xD0)))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
    }
}

fn parse_com(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag(&[0xFF, 0xFE])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take((length - 2) as usize)(input)?;
    
    Ok((input, Segment::COM(data.to_vec())))
}

fn parse_eoi(input: &[u8]) -> IResult<&[u8], Segment> {
    map(tag(&[0xFF, 0xD9]), |_| Segment::EOI)(input)
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JPEG> {
    let mut segments = Vec::new();
    let mut current_input = input;

    while !current_input.is_empty() {
        if current_input[0] == 0xFF {
            let result = nom::branch::alt((
                parse_soi,
                parse_app0,
                parse_dqt,
                parse_sof0,
                parse_dht,
                parse_sos,
                parse_dri,
                parse_rst,
                parse_com,
                parse_eoi,
            ))(current_input);

            match result {
                Ok((remaining, segment)) => {
                    current_input = remaining;
                    segments.push(segment);
                }
                Err(_) => {
                    // Handle encoded data
                    let mut pos = 1;
                    while pos < current_input.len() {
                        if current_input[pos] == 0xFF && pos + 1 < current_input.len() && current_input[pos + 1] != 0x00 {
                            break;
                        }
                        pos += 1;
                    }
                    segments.push(Segment::EncodedData(current_input[..pos].to_vec()));
                    current_input = &current_input[pos..];
                }
            }
        } else {
            // Skip non-marker bytes
            current_input = &current_input[1..];
        }
    }

    Ok((&[], JPEG { segments }))
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
        Err(e) => eprintln!("Error parsing JPEG: {:?}", e),
    }
}