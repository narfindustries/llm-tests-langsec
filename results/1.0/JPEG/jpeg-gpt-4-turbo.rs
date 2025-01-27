use nom::{
    bytes::complete::{take, tag},
    combinator::{map_res, rest},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct JPEG {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    SOI,
    APP(AppData),
    DQT(QuantizationTable),
    SOF0(FrameHeader),
    DHT(HuffmanTable),
    SOS(ScanHeader),
    EOI,
    UnknownSegment(Vec<u8>),
}

#[derive(Debug)]
struct AppData {
    marker: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct QuantizationTable {
    precision: u8,
    identifier: u8,
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
    sampling: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct HuffmanTable {
    class_and_id: u8,
    lengths: Vec<u8>,
    values: Vec<u8>,
}

#[derive(Debug)]
struct ScanHeader {
    components: Vec<ScanComponent>,
    spectral_selection: (u8, u8),
    approximation: u8,
}

#[derive(Debug)]
struct ScanComponent {
    identifier: u8,
    table_indices: u8,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <JPEG file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut content = Vec::new();
    file.read_to_end(&mut content).expect("Failed to read file");

    match parse_jpeg(&content) {
        Ok((_, jpeg)) => println!("{:#?}", jpeg),
        Err(e) => println!("Failed to parse JPEG: {:?}", e),
    }
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JPEG> {
    let (input, _) = tag(&[0xFF, 0xD8])(input)?; // SOI

    let mut segments = vec![Segment::SOI];
    let mut remaining = input;

    while !remaining.is_empty() {
        let (new_remaining, segment) = parse_segment(remaining)?;
        remaining = new_remaining;
        match &segment {
            Segment::EOI => {
                segments.push(segment);
                break;
            }
            _ => segments.push(segment),
        }
    }

    Ok((remaining, JPEG { segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = be_u16(input)?;

    match marker {
        0xFFD9 => Ok((input, Segment::EOI)),
        0xFFC0 => parse_sof(input).map(|(next_input, sof)| (next_input, Segment::SOF0(sof))),
        0xFFDB => parse_dqt(input).map(|(next_input, dqt)| (next_input, Segment::DQT(dqt))),
        0xFFC4 => parse_dht(input).map(|(next_input, dht)| (next_input, Segment::DHT(dht))),
        0xFFDA => parse_sos(input).map(|(next_input, sos)| (next_input, Segment::SOS(sos))),
        0xFFE0..=0xFFEF => parse_app(marker, input).map(|(next_input, app)| (next_input, Segment::APP(app))),
        _ => {
            let (input, data) = get_segment_data(input)?;
            Ok((input, Segment::UnknownSegment(data)))
        }
    }
}

fn get_segment_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, size) = map_res(be_u16, |s| s.checked_sub(2))(input)?;
    let (input, data) = take(size)(input)?;
    Ok((input, data.to_vec()))
}

fn parse_app(marker: u16, input: &[u8]) -> IResult<&[u8], AppData> {
    let (input, data) = get_segment_data(input)?;
    Ok((input, AppData { marker, data }))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], QuantizationTable> {
    let (input, size) = map_res(be_u16, |s| s.checked_sub(2))(input)?;
    let (input, data) = take(size)(input)?;
    let (_, mut remaining) = data;
    let mut tables = Vec::new();

    while !remaining.is_empty() {
        let (new_remaining, prec_and_id) = be_u8(remaining)?;
        let precision = (prec_and_id >> 4) & 0x0F;
        let identifier = prec_and_id & 0x0F;
        let table_length = if precision == 0 { 64 } else { 128 };
        let (new_remaining, values) = take(table_length)(new_remaining)?;

        tables.push(QuantizationTable {
            precision,
            identifier,
            values: values.to_vec(),
        });
        remaining = new_remaining;
    }

    Ok((input, tables[0].clone()))
}

fn parse_sof(input: &[u8]) -> IResult<&[u8], FrameHeader> {
    let (input, _length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let mut components = Vec::new();
    let mut remaining = input;

    for _ in 0..component_count {
        let (new_remaining, (identifier, sampling, qt_id)) = tuple((be_u8, be_u8, be_u8))(remaining)?;
        components.push(FrameComponent {
            identifier,
            sampling,
            quantization_table_id: qt_id,
        });
        remaining = new_remaining;
    }

    Ok((remaining, FrameHeader {
        precision,
        height,
        width,
        components,
    }))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], HuffmanTable> {
    let (input, size) = map_res(be_u16, |s| s.checked_sub(2))(input)?;
    let (input, data) = take(size)(input)?;
    let (_, mut remaining) = data;
    let mut tables = Vec::new();

    while !remaining.is_empty() {
        let (new_remaining, class_and_id) = be_u8(remaining)?;
        let (new_remaining, lengths) = take(16usize)(new_remaining)?;
        let total_values = lengths.iter().map(|&x| u16::from(x)).sum::<u16>() as usize;
        let (new_remaining, values) = take(total_values)(new_remaining)?;

        tables.push(HuffmanTable {
            class_and_id,
            lengths: lengths.to_vec(),
            values: values.to_vec(),
        });
        remaining = new_remaining;
    }

    Ok((input, tables[0].clone()))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], ScanHeader> {
    let (input, _length) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let mut components = Vec::new();
    let mut remaining = input;

    for _ in 0..component_count {
        let (new_remaining, (identifier, table_indices)) = tuple((be_u8, be_u8))(remaining)?;
        components.push(ScanComponent {
            identifier,
            table_indices,
        });
        remaining = new_remaining;
    }

    let (remaining, (start, end, approximation)) = tuple((be_u8, be_u8, be_u8))(remaining)?;

    Ok((remaining, ScanHeader {
        components,
        spectral_selection: (start, end),
        approximation,
    }))
}