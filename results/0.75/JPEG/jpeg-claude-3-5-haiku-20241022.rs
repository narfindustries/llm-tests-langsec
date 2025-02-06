use std::fs::File;
use std::io::Read;
use std::env;
use std::error::Error;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take},
    multi::{many0, count},
    number::complete::{
        be_u8, be_u16
    },
    combinator::map
};

#[derive(Debug)]
struct JpegHeader {
    markers: Vec<JpegMarker>
}

#[derive(Debug)]
enum JpegMarker {
    SOI,
    APP(AppMarker),
    SOF(StartOfFrame),
    DHT(HuffmanTable),
    DQT(QuantizationTable),
    DRI(RestartInterval),
    SOS(ScanHeader),
    EOI
}

#[derive(Debug)]
struct AppMarker {
    identifier: Vec<u8>
}

#[derive(Debug)]
struct StartOfFrame {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<FrameComponent>
}

#[derive(Debug)]
struct FrameComponent {
    id: u8,
    sampling_factors: u8,
    quantization_table_dest: u8
}

#[derive(Debug)]
struct HuffmanTable {
    class: u8,
    destination: u8,
    lengths: Vec<u8>,
    values: Vec<u8>
}

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    destination: u8,
    values: Vec<u8>
}

#[derive(Debug)]
struct RestartInterval {
    interval: u16
}

#[derive(Debug)]
struct ScanHeader {
    components: Vec<ScanComponent>
}

#[derive(Debug)]
struct ScanComponent {
    selector: u8,
    dc_table: u8,
    ac_table: u8
}

fn parse_soi(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    map(tag(&[0xFF, 0xD8]), |_| JpegMarker::SOI)(input)
}

fn parse_eoi(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    map(tag(&[0xFF, 0xD9]), |_| JpegMarker::EOI)(input)
}

fn parse_app_marker(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    let (input, _) = tag(&[0xFF, 0xE0])(input)?;
    let (input, _length) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;

    Ok((input, JpegMarker::APP(AppMarker {
        identifier: identifier.to_vec()
    })))
}

fn parse_sof_marker(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    let (input, _) = tag(&[0xFF, 0xC0])(input)?;
    let (input, _length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    
    let (input, components) = count(parse_frame_component, component_count as usize)(input)?;

    Ok((input, JpegMarker::SOF(StartOfFrame {
        precision,
        height,
        width,
        components
    })))
}

fn parse_frame_component(input: &[u8]) -> IResult<&[u8], FrameComponent> {
    let (input, id) = be_u8(input)?;
    let (input, sampling_factors) = be_u8(input)?;
    let (input, quantization_table_dest) = be_u8(input)?;

    Ok((input, FrameComponent {
        id,
        sampling_factors,
        quantization_table_dest
    }))
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    let (input, _) = tag(&[0xFF, 0xC4])(input)?;
    let (input, _length) = be_u16(input)?;
    let (input, class) = be_u8(input)?;
    let (input, destination) = be_u8(input)?;
    let (input, lengths) = take(16usize)(input)?;
    let total_codes: usize = lengths.iter().map(|&x| x as usize).sum();
    let (input, values) = take(total_codes)(input)?;

    Ok((input, JpegMarker::DHT(HuffmanTable {
        class,
        destination,
        lengths: lengths.to_vec(),
        values: values.to_vec()
    })))
}

fn parse_quantization_table(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    let (input, _) = tag(&[0xFF, 0xDB])(input)?;
    let (input, _length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, destination) = be_u8(input)?;
    let (input, values) = take(64usize)(input)?;

    Ok((input, JpegMarker::DQT(QuantizationTable {
        precision,
        destination,
        values: values.to_vec()
    })))
}

fn parse_restart_interval(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    let (input, _) = tag(&[0xFF, 0xDD])(input)?;
    let (input, _length) = be_u16(input)?;
    let (input, interval) = be_u16(input)?;

    Ok((input, JpegMarker::DRI(RestartInterval { interval })))
}

fn parse_scan_header(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    let (input, _) = tag(&[0xFF, 0xDA])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let (input, components) = count(parse_scan_component, component_count as usize)(input)?;
    let (input, _) = take(length as usize - 3 - (component_count as usize * 2))(input)?;

    Ok((input, JpegMarker::SOS(ScanHeader { components })))
}

fn parse_scan_component(input: &[u8]) -> IResult<&[u8], ScanComponent> {
    let (input, selector) = be_u8(input)?;
    let (input, tables) = be_u8(input)?;
    
    Ok((input, ScanComponent {
        selector,
        dc_table: tables >> 4,
        ac_table: tables & 0x0F
    }))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegHeader> {
    let (input, markers) = many0(alt((
        parse_soi,
        parse_app_marker,
        parse_sof_marker,
        parse_huffman_table,
        parse_quantization_table,
        parse_restart_interval,
        parse_scan_header,
        parse_eoi
    )))(input)?;

    Ok((input, JpegHeader { markers }))
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <jpeg_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_jpeg(&buffer) {
        Ok((_, header)) => {
            println!("Parsed JPEG: {:?}", header);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse JPEG: {:?}", e);
            std::process::exit(1);
        }
    }
}