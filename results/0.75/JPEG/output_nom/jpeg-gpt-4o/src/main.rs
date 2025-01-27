use std::fs::File;
use std::io::{self, Read};
use std::env;
use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16, be_u32},
    combinator::map,
    sequence::tuple,
    multi::many_m_n,
};

#[derive(Debug)]
struct Jpeg {
    soi: [u8; 2],
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    App0(App0Segment),
    Dqt(DqtSegment),
    Sof0(Sof0Segment),
    Dht(DhtSegment),
    Sos(SosSegment),
    Eoi,
    Unknown(u16, Vec<u8>),
}

#[derive(Debug)]
struct App0Segment {
    length: u16,
    identifier: [u8; 5],
    version: u16,
    units: u8,
    x_density: u16,
    y_density: u16,
    x_thumbnail: u8,
    y_thumbnail: u8,
    thumbnail_data: Vec<u8>,
}

#[derive(Debug)]
struct DqtSegment {
    length: u16,
    qt_info: Vec<u8>,
}

#[derive(Debug)]
struct Sof0Segment {
    length: u16,
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<Component>,
}

#[derive(Debug)]
struct Component {
    id: u8,
    sampling_factors: u8,
    qt_selector: u8,
}

#[derive(Debug)]
struct DhtSegment {
    length: u16,
    ht_info: Vec<u8>,
}

#[derive(Debug)]
struct SosSegment {
    length: u16,
    components: Vec<SosComponent>,
    ss: u8,
    se: u8,
    ah_al: u8,
    compressed_data: Vec<u8>,
}

#[derive(Debug)]
struct SosComponent {
    id: u8,
    huffman_table: u8,
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Jpeg> {
    let (input, soi) = tag([0xFF, 0xD8])(input)?;
    let (input, segments) = many_m_n(0, usize::MAX, parse_segment)(input)?;
    Ok((input, Jpeg { soi: [0xFF, 0xD8], segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = tuple((tag([0xFF]), be_u8))(input)?;
    match marker.1 {
        0xE0 => map(parse_app0, Segment::App0)(input),
        0xDB => map(parse_dqt, Segment::Dqt)(input),
        0xC0 => map(parse_sof0, Segment::Sof0)(input),
        0xC4 => map(parse_dht, Segment::Dht)(input),
        0xDA => map(parse_sos, Segment::Sos)(input),
        0xD9 => Ok((input, Segment::Eoi)),
        _ => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::Unknown(marker.1 as u16, data.to_vec())))
        },
    }
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], App0Segment> {
    let (input, (length, identifier, version, units, x_density, y_density, x_thumbnail, y_thumbnail)) = 
        tuple((be_u16, take(5usize), be_u16, be_u8, be_u16, be_u16, be_u8, be_u8))(input)?;
    let thumbnail_length = (x_thumbnail as usize) * (y_thumbnail as usize) * 3;
    let (input, thumbnail_data) = take(thumbnail_length)(input)?;
    Ok((input, App0Segment {
        length,
        identifier: identifier.try_into().unwrap(),
        version,
        units,
        x_density,
        y_density,
        x_thumbnail,
        y_thumbnail,
        thumbnail_data: thumbnail_data.to_vec(),
    }))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], DqtSegment> {
    let (input, length) = be_u16(input)?;
    let (input, qt_info) = take(length - 2)(input)?;
    Ok((input, DqtSegment { length, qt_info: qt_info.to_vec() }))
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], Sof0Segment> {
    let (input, (length, precision, height, width, component_count)) = tuple((be_u16, be_u8, be_u16, be_u16, be_u8))(input)?;
    let (input, components) = many_m_n(component_count as usize, component_count as usize, parse_component)(input)?;
    Ok((input, Sof0Segment { length, precision, height, width, components }))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, (id, sampling_factors, qt_selector)) = tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((input, Component { id, sampling_factors, qt_selector }))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], DhtSegment> {
    let (input, length) = be_u16(input)?;
    let (input, ht_info) = take(length - 2)(input)?;
    Ok((input, DhtSegment { length, ht_info: ht_info.to_vec() }))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], SosSegment> {
    let (input, (length, component_count)) = tuple((be_u16, be_u8))(input)?;
    let (input, components) = many_m_n(component_count as usize, component_count as usize, parse_sos_component)(input)?;
    let (input, (ss, se, ah_al)) = tuple((be_u8, be_u8, be_u8))(input)?;
    let (input, compressed_data) = take(length - 2 - 1 - (component_count as usize * 2) - 3)(input)?;
    Ok((input, SosSegment { length, components, ss, se, ah_al, compressed_data: compressed_data.to_vec() }))
}

fn parse_sos_component(input: &[u8]) -> IResult<&[u8], SosComponent> {
    let (input, (id, huffman_table)) = tuple((be_u8, be_u8))(input)?;
    Ok((input, SosComponent { id, huffman_table }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <jpeg_file>", args[0]);
        std::process::exit(1);
    }
    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    
    match parse_jpeg(&buffer) {
        Ok((_, jpeg)) => println!("{:#?}", jpeg),
        Err(e) => eprintln!("Failed to parse JPEG file: {:?}", e),
    }

    Ok(())
}