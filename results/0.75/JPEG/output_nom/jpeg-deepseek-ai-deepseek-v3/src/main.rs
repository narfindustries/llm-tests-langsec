use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::many0,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult, error::{Error, ErrorKind},
};
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
    EOI,
    APP(u8, Vec<u8>),
    DQT(Vec<QuantizationTable>),
    SOF(StartOfFrame),
    DHT(Vec<HuffmanTable>),
    SOS(StartOfScan),
    DRI(u16),
    COM(Vec<u8>),
    RST(u8),
}

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    id: u8,
    table: Vec<u8>,
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
    sampling: (u8, u8),
    quant_id: u8,
}

#[derive(Debug)]
struct HuffmanTable {
    class: u8,
    id: u8,
    codes: Vec<u8>,
    values: Vec<u8>,
}

#[derive(Debug)]
struct StartOfScan {
    components: Vec<ScanComponent>,
    spectral_start: u8,
    spectral_end: u8,
    successive_approx: (u8, u8),
}

#[derive(Debug)]
struct ScanComponent {
    id: u8,
    dc_table: u8,
    ac_table: u8,
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JPEG> {
    let (input, segments) = many0(parse_segment)(input)?;
    Ok((input, JPEG { segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = parse_marker(input)?;
    match marker {
        0xFFD8 => Ok((input, Segment::SOI)),
        0xFFD9 => Ok((input, Segment::EOI)),
        0xFFDD => parse_dri(input),
        0xFFDA => parse_sos(input),
        0xFFC4 => parse_dht(input),
        0xFFDB => parse_dqt(input),
        0xFFC0..=0xFFCF => parse_sof(input, marker),
        0xFFE0..=0xFFEF => parse_app(input, marker),
        0xFFFE => parse_com(input),
        0xFFD0..=0xFFD7 => Ok((input, Segment::RST((marker - 0xFFD0) as u8))),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::Tag))),
    }
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], u16> {
    let (input, _) = tag([0xFF])(input)?;
    be_u16(input)
}

fn parse_dri(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _length) = be_u16(input)?;
    let (input, interval) = be_u16(input)?;
    Ok((input, Segment::DRI(interval)))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _length) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, components) = take(num_components as usize)(input)?;
    let (input, spectral_start) = be_u8(input)?;
    let (input, spectral_end) = be_u8(input)?;
    let (input, successive_approx) = be_u8(input)?;

    let components = components
        .chunks(2)
        .map(|chunk| ScanComponent {
            id: chunk[0],
            dc_table: chunk[1] >> 4,
            ac_table: chunk[1] & 0x0F,
        })
        .collect();

    Ok((
        input,
        Segment::SOS(StartOfScan {
            components,
            spectral_start,
            spectral_end,
            successive_approx: (successive_approx >> 4, successive_approx & 0x0F),
        }),
    ))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _length) = be_u16(input)?;
    let (input, tables) = many0(parse_huffman_table)(input)?;
    Ok((input, Segment::DHT(tables)))
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, class_and_id) = be_u8(input)?;
    let (input, codes) = take(16usize)(input)?;
    let (input, values) = take(codes.iter().map(|&x| x as usize).sum::<usize>())(input)?;

    Ok((
        input,
        HuffmanTable {
            class: class_and_id >> 4,
            id: class_and_id & 0x0F,
            codes: codes.to_vec(),
            values: values.to_vec(),
        },
    ))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _length) = be_u16(input)?;
    let (input, tables) = many0(parse_quantization_table)(input)?;
    Ok((input, Segment::DQT(tables)))
}

fn parse_quantization_table(input: &[u8]) -> IResult<&[u8], QuantizationTable> {
    let (input, precision_and_id) = be_u8(input)?;
    let (input, table) = take(64usize)(input)?;

    Ok((
        input,
        QuantizationTable {
            precision: precision_and_id >> 4,
            id: precision_and_id & 0x0F,
            table: table.to_vec(),
        },
    ))
}

fn parse_sof(input: &[u8], _marker: u16) -> IResult<&[u8], Segment> {
    let (input, _length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, components) = take(num_components as usize * 3)(input)?;

    let components = components
        .chunks(3)
        .map(|chunk| FrameComponent {
            id: chunk[0],
            sampling: (chunk[1] >> 4, chunk[1] & 0x0F),
            quant_id: chunk[2],
        })
        .collect();

    Ok((
        input,
        Segment::SOF(StartOfFrame {
            precision,
            height,
            width,
            components,
        }),
    ))
}

fn parse_app(input: &[u8], marker: u16) -> IResult<&[u8], Segment> {
    let (input, _length) = be_u16(input)?;
    let (input, identifier_and_data) = take(_length - 2)(input)?;
    Ok((
        input,
        Segment::APP((marker - 0xFFE0) as u8, identifier_and_data.to_vec()),
    ))
}

fn parse_com(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _length) = be_u16(input)?;
    let (input, comment) = take(_length - 2)(input)?;
    Ok((input, Segment::COM(comment.to_vec())))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
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