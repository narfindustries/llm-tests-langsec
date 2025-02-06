use nom::{
    bytes::complete::take,
    combinator::map_res,
    multi::many0,
    number::complete::{be_u8, be_u16},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct JPEG {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    SOI,
    EOI,
    APPn(u8, Vec<u8>),
    COM(Vec<u8>),
    DQT(Vec<QuantizationTable>),
    DHT(Vec<HuffmanTable>),
    SOFn(SOF),
    SOS(SOS),
    DRI(u16),
    DNL(u16),
    RSTn(u8),
}

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    id: u8,
    table: Vec<u16>,
}

#[derive(Debug)]
struct HuffmanTable {
    class: u8,
    id: u8,
    codes: Vec<u8>,
    symbols: Vec<u8>,
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
    sampling_factors: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct SOS {
    components: Vec<SOSComponent>,
    spectral_selection_start: u8,
    spectral_selection_end: u8,
    successive_approximation: u8,
}

#[derive(Debug)]
struct SOSComponent {
    id: u8,
    huffman_table_ids: u8,
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JPEG> {
    let (input, segments) = many0(parse_segment)(input)?;
    Ok((input, JPEG { segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = be_u16(input)?;
    match marker {
        0xFFD8 => Ok((input, Segment::SOI)),
        0xFFD9 => Ok((input, Segment::EOI)),
        0xFFE0..=0xFFEF => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::APPn((marker & 0x0F) as u8, data.to_vec())))
        }
        0xFFFE => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length - 2)(input)?;
            Ok((input, Segment::COM(data.to_vec())))
        }
        0xFFDB => {
            let (input, tables) = many0(parse_quantization_table)(input)?;
            Ok((input, Segment::DQT(tables)))
        }
        0xFFC4 => {
            let (input, tables) = many0(parse_huffman_table)(input)?;
            Ok((input, Segment::DHT(tables)))
        }
        0xFFC0..=0xFFCF => {
            let (input, sof) = parse_sof(input)?;
            Ok((input, Segment::SOFn(sof)))
        }
        0xFFDA => {
            let (input, sos) = parse_sos(input)?;
            Ok((input, Segment::SOS(sos)))
        }
        0xFFDD => {
            let (input, interval) = be_u16(input)?;
            Ok((input, Segment::DRI(interval)))
        }
        0xFFDC => {
            let (input, lines) = be_u16(input)?;
            Ok((input, Segment::DNL(lines)))
        }
        0xFFD0..=0xFFD7 => Ok((input, Segment::RSTn((marker & 0x0F) as u8))),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_quantization_table(input: &[u8]) -> IResult<&[u8], QuantizationTable> {
    let (input, precision_and_id) = be_u8(input)?;
    let precision = (precision_and_id >> 4) & 0x0F;
    let id = precision_and_id & 0x0F;
    let (input, table) = if precision == 0 {
        map_res(take(64usize), |data: &[u8]| Ok::<_, nom::error::Error<&[u8]>>(data.iter().map(|&x| x as u16).collect::<Vec<u16>>()))(input)?
    } else {
        map_res(take(128usize), |data: &[u8]| {
            Ok::<_, nom::error::Error<&[u8]>>(data.chunks(2).map(|chunk| ((chunk[0] as u16) << 8) | chunk[1] as u16).collect::<Vec<u16>>())
        })(input)?
    };
    Ok((input, QuantizationTable { precision, id, table }))
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, class_and_id) = be_u8(input)?;
    let class = (class_and_id >> 4) & 0x0F;
    let id = class_and_id & 0x0F;
    let (input, codes) = take(16usize)(input)?;
    let (input, symbols) = take(codes.iter().map(|&x| x as usize).sum::<usize>())(input)?;
    Ok((input, HuffmanTable { class, id, codes: codes.to_vec(), symbols: symbols.to_vec() }))
}

fn parse_sof(input: &[u8]) -> IResult<&[u8], SOF> {
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, _num_components) = be_u8(input)?;
    let (input, components) = many0(parse_component)(input)?;
    Ok((input, SOF { precision, height, width, components }))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, id) = be_u8(input)?;
    let (input, sampling_factors) = be_u8(input)?;
    let (input, quantization_table_id) = be_u8(input)?;
    Ok((input, Component { id, sampling_factors, quantization_table_id }))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], SOS> {
    let (input, _num_components) = be_u8(input)?;
    let (input, components) = many0(parse_sos_component)(input)?;
    let (input, spectral_selection_start) = be_u8(input)?;
    let (input, spectral_selection_end) = be_u8(input)?;
    let (input, successive_approximation) = be_u8(input)?;
    Ok((input, SOS { components, spectral_selection_start, spectral_selection_end, successive_approximation }))
}

fn parse_sos_component(input: &[u8]) -> IResult<&[u8], SOSComponent> {
    let (input, id) = be_u8(input)?;
    let (input, huffman_table_ids) = be_u8(input)?;
    Ok((input, SOSComponent { id, huffman_table_ids }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_jpeg(&buffer) {
        Ok((_, jpeg)) => println!("{:#?}", jpeg),
        Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
    }
}