use nom::{
    bytes::complete::{tag, take},
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
    SOF(StartOfFrame),
    DQT(QuantizationTable),
    DHT(HuffmanTable),
    SOS(StartOfScan),
    DRI(RestartInterval),
    APP(ApplicationData),
    COM(Comment),
    Data(Vec<u8>),
}

#[derive(Debug)]
struct StartOfFrame {
    marker_type: u8,
    length: u16,
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<FrameComponent>,
}

#[derive(Debug)]
struct FrameComponent {
    id: u8,
    h_sampling: u8,
    v_sampling: u8,
    qtable_id: u8,
}

#[derive(Debug)]
struct QuantizationTable {
    length: u16,
    tables: Vec<QTable>,
}

#[derive(Debug)]
struct QTable {
    precision_and_id: u8,
    values: Vec<u8>,
}

#[derive(Debug)]
struct HuffmanTable {
    length: u16,
    tables: Vec<HTable>,
}

#[derive(Debug)]
struct HTable {
    class_and_id: u8,
    counts: Vec<u8>,
    values: Vec<u8>,
}

#[derive(Debug)]
struct StartOfScan {
    length: u16,
    component_count: u8,
    components: Vec<ScanComponent>,
    spectral_start: u8,
    spectral_end: u8,
    approx: u8,
}

#[derive(Debug)]
struct ScanComponent {
    id: u8,
    dc_ac_tables: u8,
}

#[derive(Debug)]
struct RestartInterval {
    length: u16,
    interval: u16,
}

#[derive(Debug)]
struct ApplicationData {
    marker: u8,
    length: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct Comment {
    length: u16,
    data: Vec<u8>,
}

fn parse_soi(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag(&[0xFF, 0xD8])(input)?;
    Ok((input, Segment::SOI))
}

fn parse_eoi(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, _) = tag(&[0xFF, 0xD9])(input)?;
    Ok((input, Segment::EOI))
}

fn parse_frame_component(input: &[u8]) -> IResult<&[u8], FrameComponent> {
    let (input, (id, sampling, qtable_id)) = tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((
        input,
        FrameComponent {
            id,
            h_sampling: sampling >> 4,
            v_sampling: sampling & 0x0F,
            qtable_id,
        },
    ))
}

fn parse_sof(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, (marker_type, length, precision, height, width, component_count)) =
        tuple((be_u8, be_u16, be_u8, be_u16, be_u16, be_u8))(input)?;
    
    let mut components = Vec::new();
    let mut remaining = input;
    
    for _ in 0..component_count {
        let (new_input, component) = parse_frame_component(remaining)?;
        components.push(component);
        remaining = new_input;
    }
    
    Ok((
        remaining,
        Segment::SOF(StartOfFrame {
            marker_type,
            length,
            precision,
            height,
            width,
            components,
        }),
    ))
}

fn parse_qtable(input: &[u8]) -> IResult<&[u8], QTable> {
    let (input, precision_and_id) = be_u8(input)?;
    let precision = precision_and_id >> 4;
    let count = if precision == 0 { 64usize } else { 128usize };
    let (input, values) = take(count)(input)?;
    
    Ok((
        input,
        QTable {
            precision_and_id,
            values: values.to_vec(),
        },
    ))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], Segment> {
    let (mut input, length) = be_u16(input)?;
    let mut remaining_length = length as usize - 2;
    let mut tables = Vec::new();
    
    while remaining_length > 0 {
        let (new_input, table) = parse_qtable(input)?;
        let consumed = input.len() - new_input.len();
        remaining_length -= consumed;
        input = new_input;
        tables.push(table);
    }
    
    Ok((input, Segment::DQT(QuantizationTable { length, tables })))
}

fn parse_htable(input: &[u8]) -> IResult<&[u8], HTable> {
    let (input, class_and_id) = be_u8(input)?;
    let (input, counts) = take(16usize)(input)?;
    let count: usize = counts.iter().map(|&x| x as usize).sum();
    let (input, values) = take(count)(input)?;
    
    Ok((
        input,
        HTable {
            class_and_id,
            counts: counts.to_vec(),
            values: values.to_vec(),
        },
    ))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], Segment> {
    let (mut input, length) = be_u16(input)?;
    let mut remaining_length = length as usize - 2;
    let mut tables = Vec::new();
    
    while remaining_length > 0 {
        let (new_input, table) = parse_htable(input)?;
        let consumed = input.len() - new_input.len();
        remaining_length -= consumed;
        input = new_input;
        tables.push(table);
    }
    
    Ok((input, Segment::DHT(HuffmanTable { length, tables })))
}

fn parse_scan_component(input: &[u8]) -> IResult<&[u8], ScanComponent> {
    let (input, (id, dc_ac_tables)) = tuple((be_u8, be_u8))(input)?;
    Ok((input, ScanComponent { id, dc_ac_tables }))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, (length, component_count)) = tuple((be_u16, be_u8))(input)?;
    let mut components = Vec::new();
    let mut remaining = input;
    
    for _ in 0..component_count {
        let (new_input, component) = parse_scan_component(remaining)?;
        components.push(component);
        remaining = new_input;
    }
    
    let (remaining, (spectral_start, spectral_end, approx)) =
        tuple((be_u8, be_u8, be_u8))(remaining)?;
    
    Ok((
        remaining,
        Segment::SOS(StartOfScan {
            length,
            component_count,
            components,
            spectral_start,
            spectral_end,
            approx,
        }),
    ))
}

fn parse_dri(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, (length, interval)) = tuple((be_u16, be_u16))(input)?;
    Ok((input, Segment::DRI(RestartInterval { length, interval })))
}

fn parse_app(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = be_u8(input)?;
    let (input, length) = be_u16(input)?;
    let data_length = length as usize - 2;
    let (input, data) = take(data_length)(input)?;
    Ok((
        input,
        Segment::APP(ApplicationData {
            marker,
            length,
            data: data.to_vec(),
        }),
    ))
}

fn parse_com(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, length) = be_u16(input)?;
    let data_length = length as usize - 2;
    let (input, data) = take(data_length)(input)?;
    Ok((
        input,
        Segment::COM(Comment {
            length,
            data: data.to_vec(),
        }),
    ))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegImage> {
    let mut segments = Vec::new();
    let mut remaining = input;
    
    while !remaining.is_empty() {
        if remaining.len() >= 2 && remaining[0] == 0xFF {
            let marker = remaining[1];
            let (new_input, segment) = match marker {
                0xD8 => parse_soi(remaining)?,
                0xD9 => parse_eoi(remaining)?,
                0xC0..=0xC3 | 0xC5..=0xCF => {
                    let (input, _) = tag(&[0xFF])(remaining)?;
                    parse_sof(input)?
                }
                0xDB => {
                    let (input, _) = tag(&[0xFF])(remaining)?;
                    parse_dqt(input)?
                }
                0xC4 => {
                    let (input, _) = tag(&[0xFF])(remaining)?;
                    parse_dht(input)?
                }
                0xDA => {
                    let (input, _) = tag(&[0xFF])(remaining)?;
                    parse_sos(input)?
                }
                0xDD => {
                    let (input, _) = tag(&[0xFF])(remaining)?;
                    parse_dri(input)?
                }
                0xE0..=0xEF => {
                    let (input, _) = tag(&[0xFF])(remaining)?;
                    parse_app(input)?
                }
                0xFE => {
                    let (input, _) = tag(&[0xFF])(remaining)?;
                    parse_com(input)?
                }
                _ => {
                    let (input, data) = take(1usize)(remaining)?;
                    (input, Segment::Data(data.to_vec()))
                }
            };
            segments.push(segment);
            remaining = new_input;
        } else {
            let (input, data) = take(1usize)(remaining)?;
            segments.push(Segment::Data(data.to_vec()));
            remaining = input;
        }
    }
    
    Ok((remaining, JpegImage { segments }))
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
        Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
    }
}