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
    EOI,
    APP(u8, Vec<u8>),
    DQT(Vec<QuantizationTable>),
    SOF0(StartOfFrame),
    DHT(Vec<HuffmanTable>),
    SOS(StartOfScan),
    COM(Vec<u8>),
    Data(Vec<u8>),
}

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    id: u8,
    values: Vec<u8>,
}

#[derive(Debug)]
struct StartOfFrame {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<FrameComponent>,
}

#[derive(Debug)]
struct FrameComponent {
    id: u8,
    sampling_factors: u8,
    qt_id: u8,
}

#[derive(Debug)]
struct HuffmanTable {
    class: u8,
    id: u8,
    values: Vec<u8>,
}

#[derive(Debug)]
struct StartOfScan {
    components: Vec<ScanComponent>,
    start_spectral: u8,
    end_spectral: u8,
    approx_bits: u8,
}

#[derive(Debug)]
struct ScanComponent {
    id: u8,
    huffman_table_ids: u8,
}

fn parse_soi(input: &[u8]) -> IResult<&[u8], Segment> {
    map(tag([0xFF, 0xD8]), |_| Segment::SOI)(input)
}

fn parse_eoi(input: &[u8]) -> IResult<&[u8], Segment> {
    map(tag([0xFF, 0xD9]), |_| Segment::EOI)(input)
}

fn parse_app(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = verify(be_u8, |x| (0xE0..=0xEF).contains(x))(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length as usize - 2)(input)?;
    Ok((input, Segment::APP(marker - 0xE0, data.to_vec())))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag([0xFF, 0xDB])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, tables) = take(length as usize - 2)(input)?;
    let mut tables_vec = Vec::new();
    let mut tables_input = tables;
    
    while !tables_input.is_empty() {
        let (rest, info) = be_u8(tables_input)?;
        let precision = (info >> 4) & 0x0F;
        let id = info & 0x0F;
        let (rest, values) = take(64)(rest)?;
        tables_vec.push(QuantizationTable {
            precision,
            id,
            values: values.to_vec(),
        });
        tables_input = rest;
    }
    
    Ok((input, Segment::DQT(tables_vec)))
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag([0xFF, 0xC0])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let mut components = Vec::new();
    
    let (input, components_data) = take(component_count as usize * 3)(input)?;
    let mut components_input = components_data;
    
    while !components_input.is_empty() {
        let (rest, id) = be_u8(components_input)?;
        let (rest, sampling_factors) = be_u8(rest)?;
        let (rest, qt_id) = be_u8(rest)?;
        components.push(FrameComponent {
            id,
            sampling_factors,
            qt_id,
        });
        components_input = rest;
    }
    
    Ok((input, Segment::SOF0(StartOfFrame {
        precision,
        height,
        width,
        components,
    })))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag([0xFF, 0xC4])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, tables_data) = take(length as usize - 2)(input)?;
    let mut tables = Vec::new();
    let mut tables_input = tables_data;
    
    while !tables_input.is_empty() {
        let (rest, info) = be_u8(tables_input)?;
        let class = (info >> 4) & 0x0F;
        let id = info & 0x0F;
        let (rest, lengths) = take(16usize)(rest)?;
        let total_codes: usize = lengths.iter().map(|&x| x as usize).sum();
        let (rest, values) = take(total_codes)(rest)?;
        tables.push(HuffmanTable {
            class,
            id,
            values: values.to_vec(),
        });
        tables_input = rest;
    }
    
    Ok((input, Segment::DHT(tables)))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag([0xFF, 0xDA])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let mut components = Vec::new();
    
    let (input, components_data) = take(component_count as usize * 2)(input)?;
    let mut components_input = components_data;
    
    while !components_input.is_empty() {
        let (rest, id) = be_u8(components_input)?;
        let (rest, huffman_table_ids) = be_u8(rest)?;
        components.push(ScanComponent {
            id,
            huffman_table_ids,
        });
        components_input = rest;
    }
    
    let (input, start_spectral) = be_u8(input)?;
    let (input, end_spectral) = be_u8(input)?;
    let (input, approx_bits) = be_u8(input)?;
    
    Ok((input, Segment::SOS(StartOfScan {
        components,
        start_spectral,
        end_spectral,
        approx_bits,
    })))
}

fn parse_com(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag([0xFF, 0xFE])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length as usize - 2)(input)?;
    Ok((input, Segment::COM(data.to_vec())))
}

fn parse_data(input: &[u8]) -> IResult<&[u8], Segment> {
    let mut data = Vec::new();
    let mut current_input = input;
    
    loop {
        if current_input.len() < 2 {
            break;
        }
        
        if current_input[0] == 0xFF {
            if current_input[1] == 0x00 {
                data.push(0xFF);
                current_input = &current_input[2..];
            } else if current_input[1] == 0xFF {
                data.push(0xFF);
                current_input = &current_input[1..];
            } else {
                break;
            }
        } else {
            data.push(current_input[0]);
            current_input = &current_input[1..];
        }
    }
    
    Ok((current_input, Segment::Data(data)))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    if input.len() < 2 {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Eof,
        )));
    }
    
    match (input[0], input[1]) {
        (0xFF, 0xD8) => parse_soi(input),
        (0xFF, 0xD9) => parse_eoi(input),
        (0xFF, m) if (0xE0..=0xEF).contains(&m) => parse_app(&input[1..]),
        (0xFF, 0xDB) => parse_dqt(input),
        (0xFF, 0xC0) => parse_sof0(input),
        (0xFF, 0xC4) => parse_dht(input),
        (0xFF, 0xDA) => parse_sos(input),
        (0xFF, 0xFE) => parse_com(input),
        _ => parse_data(input),
    }
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegImage> {
    map(many0(parse_segment), |segments| JpegImage { segments })(input)
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
        Ok((remaining, jpeg)) => {
            if !remaining.is_empty() {
                println!("Warning: {} unparsed bytes remaining", remaining.len());
            }
            println!("{:#?}", jpeg);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing JPEG: {:?}", e);
            std::process::exit(1);
        }
    }
}