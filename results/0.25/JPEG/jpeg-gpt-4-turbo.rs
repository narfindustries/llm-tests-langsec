use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::{count, many0},
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::path::PathBuf;
use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Opt {
    #[clap(parse(from_os_str))]
    input: PathBuf,
}

#[derive(Debug)]
struct JPEG {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    SOI,
    EOI,
    APP(Vec<u8>),
    COM(Vec<u8>),
    DQT(Vec<QuantizationTable>),
    DHT(Vec<HuffmanTable>),
    SOF0(FrameHeader),
    SOS(ScanHeader),
    DRI(u16),
    Unknown(Vec<u8>),
}

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    identifier: u8,
    values: Vec<u8>,
}

#[derive(Debug)]
struct HuffmanTable {
    class: u8,
    identifier: u8,
    lengths: Vec<u8>,
    values: Vec<u8>,
}

#[derive(Debug)]
struct FrameHeader {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<FrameComponent>,
}

#[derive(Debug)]
struct FrameComponent {
    identifier: u8,
    sampling_factor: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct ScanHeader {
    components: Vec<ScanComponent>,
    start: u8,
    end: u8,
    approximation: u8,
}

#[derive(Debug)]
struct ScanComponent {
    identifier: u8,
    huffman_table_id: u8,
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JPEG> {
    let (input, segments) = many0(parse_segment)(input)?;
    Ok((input, JPEG { segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = preceded(tag(b"\xFF"), be_u8)(input)?;
    match marker {
        0xD8 => Ok((input, Segment::SOI)),
        0xD9 => Ok((input, Segment::EOI)),
        0xE0..=0xEF => map(preceded(be_u16, |i| take(i - 2)), Segment::APP)(input),
        0xFE => map(preceded(be_u16, |i| take(i - 2)), Segment::COM)(input),
        0xDB => map(preceded(be_u16, parse_dqt), Segment::DQT)(input),
        0xC4 => map(preceded(be_u16, parse_dht), Segment::DHT)(input),
        0xC0 => map(preceded(be_u16, parse_sof0), Segment::SOF0)(input),
        0xDA => map(preceded(be_u16, parse_sos), Segment::SOS)(input),
        0xDD => map(preceded(be_u16, be_u16), Segment::DRI)(input),
        _ => map(preceded(be_u16, |i| take(i - 2)), Segment::Unknown)(input),
    }
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], Vec<QuantizationTable>> {
    let (input, count) = be_u8(input)?;
    count(parse_quantization_table, count as usize)(input)
}

fn parse_quantization_table(input: &[u8]) -> IResult<&[u8], QuantizationTable> {
    let (input, (info, values)) = tuple((be_u8, count(be_u8, 64)))(input)?;
    let precision = (info >> 4) & 0x0F;
    let identifier = info & 0x0F;
    Ok((input, QuantizationTable { precision, identifier, values }))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], Vec<HuffmanTable>> {
    let (input, count) = be_u8(input)?;
    count(parse_huffman_table, count as usize)(input)
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, info) = be_u8(input)?;
    let class = (info >> 4) & 0x0F;
    let identifier = info & 0x0F;
    let (input, lengths) = count(be_u8, 16)(input)?;
    let total_values = lengths.iter().sum::<u8>() as usize;
    let (input, values) = count(be_u8, total_values)(input)?;
    Ok((input, HuffmanTable { class, identifier, lengths, values }))
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], FrameHeader> {
    let (input, (precision, height, width, component_count)) = tuple((be_u8, be_u16, be_u16, be_u8))(input)?;
    let (input, components) = count(parse_frame_component, component_count as usize)(input)?;
    Ok((input, FrameHeader { precision, height, width, components }))
}

fn parse_frame_component(input: &[u8]) -> IResult<&[u8], FrameComponent> {
    let (input, (identifier, sampling_factor, quantization_table_id)) = tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((input, FrameComponent { identifier, sampling_factor, quantization_table_id }))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], ScanHeader> {
    let (input, component_count) = be_u8(input)?;
    let (input, components) = count(parse_scan_component, component_count as usize)(input)?;
    let (input, (start, end, approximation)) = tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((input, ScanHeader { components, start, end, approximation }))
}

fn parse_scan_component(input: &[u8]) -> IResult<&[u8], ScanComponent> {
    let (input, (identifier, huffman_table_id)) = tuple((be_u8, be_u8))(input)?;
    Ok((input, ScanComponent { identifier, huffman_table_id }))
}

fn main() -> io::Result<()> {
    let opt = Opt::parse();
    let mut file = File::open(opt.input)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_jpeg(&buffer) {
        Ok((_, jpeg)) => println!("{:#?}", jpeg),
        Err(e) => println!("Failed to parse JPEG: {:?}", e),
    }

    Ok(())
}