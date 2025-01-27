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
    Start,
    App0(App0Segment),
    App1(Vec<u8>),
    Comment(Vec<u8>),
    DQT(Vec<QuantizationTable>),
    SOF0(StartOfFrame0),
    DHT(Vec<HuffmanTable>),
    SOS(StartOfScan),
    Data(Vec<u8>),
    End,
}

#[derive(Debug)]
struct App0Segment {
    identifier: Vec<u8>,
    version_major: u8,
    version_minor: u8,
    density_units: u8,
    x_density: u16,
    y_density: u16,
    thumbnail_width: u8,
    thumbnail_height: u8,
    thumbnail_data: Vec<u8>,
}

#[derive(Debug)]
struct QuantizationTable {
    precision_and_id: u8,
    values: Vec<u8>,
}

#[derive(Debug)]
struct StartOfFrame0 {
    precision: u8,
    height: u16,
    width: u16,
    components: Vec<FrameComponent>,
}

#[derive(Debug)]
struct FrameComponent {
    id: u8,
    sampling_factors: u8,
    qt_table_id: u8,
}

#[derive(Debug)]
struct HuffmanTable {
    class_and_id: u8,
    counts: Vec<u8>,
    values: Vec<u8>,
}

#[derive(Debug)]
struct StartOfScan {
    component_count: u8,
    components: Vec<ScanComponent>,
    start_spectral: u8,
    end_spectral: u8,
    approx_bits: u8,
}

#[derive(Debug)]
struct ScanComponent {
    id: u8,
    dc_ac_tables: u8,
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], App0Segment> {
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;
    let (input, (version_major, version_minor, density_units, x_density, y_density, thumbnail_width, thumbnail_height)) =
        tuple((be_u8, be_u8, be_u8, be_u16, be_u16, be_u8, be_u8))(input)?;
    let thumbnail_size = thumbnail_width as usize * thumbnail_height as usize * 3;
    let (input, thumbnail_data) = take(thumbnail_size)(input)?;

    Ok((
        input,
        App0Segment {
            identifier: identifier.to_vec(),
            version_major,
            version_minor,
            density_units,
            x_density,
            y_density,
            thumbnail_width,
            thumbnail_height,
            thumbnail_data: thumbnail_data.to_vec(),
        },
    ))
}

fn parse_quantization_table(input: &[u8]) -> IResult<&[u8], Vec<QuantizationTable>> {
    let (input, length) = be_u16(input)?;
    let mut remaining = &input[..(length as usize - 2)];
    let mut tables = Vec::new();

    while !remaining.is_empty() {
        let (rest, precision_and_id) = be_u8(remaining)?;
        let (rest, values) = take(64usize)(rest)?;
        tables.push(QuantizationTable {
            precision_and_id,
            values: values.to_vec(),
        });
        remaining = rest;
    }

    Ok((&input[(length as usize - 2)..], tables))
}

fn parse_frame_component(input: &[u8]) -> IResult<&[u8], FrameComponent> {
    let (input, (id, sampling_factors, qt_table_id)) = tuple((be_u8, be_u8, be_u8))(input)?;
    Ok((
        input,
        FrameComponent {
            id,
            sampling_factors,
            qt_table_id,
        },
    ))
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], StartOfFrame0> {
    let (input, length) = be_u16(input)?;
    let (input, (precision, height, width, component_count)) =
        tuple((be_u8, be_u16, be_u16, be_u8))(input)?;
    let (input, components) = many0(parse_frame_component)(input)?;

    Ok((
        input,
        StartOfFrame0 {
            precision,
            height,
            width,
            components,
        },
    ))
}

fn parse_huffman_table(input: &[u8]) -> IResult<&[u8], Vec<HuffmanTable>> {
    let (input, length) = be_u16(input)?;
    let mut remaining = &input[..(length as usize - 2)];
    let mut tables = Vec::new();

    while !remaining.is_empty() {
        let (rest, class_and_id) = be_u8(remaining)?;
        let (rest, counts) = take(16usize)(rest)?;
        let sum: usize = counts.iter().map(|&x| x as usize).sum();
        let (rest, values) = take(sum)(rest)?;
        
        tables.push(HuffmanTable {
            class_and_id,
            counts: counts.to_vec(),
            values: values.to_vec(),
        });
        remaining = rest;
    }

    Ok((&input[(length as usize - 2)..], tables))
}

fn parse_scan_component(input: &[u8]) -> IResult<&[u8], ScanComponent> {
    let (input, (id, dc_ac_tables)) = tuple((be_u8, be_u8))(input)?;
    Ok((
        input,
        ScanComponent {
            id,
            dc_ac_tables,
        },
    ))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], StartOfScan> {
    let (input, length) = be_u16(input)?;
    let (input, component_count) = be_u8(input)?;
    let (input, components) = many0(parse_scan_component)(input)?;
    let (input, (start_spectral, end_spectral, approx_bits)) =
        tuple((be_u8, be_u8, be_u8))(input)?;

    Ok((
        input,
        StartOfScan {
            component_count,
            components,
            start_spectral,
            end_spectral,
            approx_bits,
        },
    ))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, marker) = be_u8(input)?;
    match marker {
        0xFF => {
            let (input, segment_type) = be_u8(input)?;
            match segment_type {
                0xD8 => Ok((input, Segment::Start)),
                0xE0 => map(parse_app0, Segment::App0)(input),
                0xE1 => {
                    let (input, length) = be_u16(input)?;
                    let (input, data) = take(length as usize - 2)(input)?;
                    Ok((input, Segment::App1(data.to_vec())))
                }
                0xFE => {
                    let (input, length) = be_u16(input)?;
                    let (input, data) = take(length as usize - 2)(input)?;
                    Ok((input, Segment::Comment(data.to_vec())))
                }
                0xDB => map(parse_quantization_table, Segment::DQT)(input),
                0xC0 => map(parse_sof0, Segment::SOF0)(input),
                0xC4 => map(parse_huffman_table, Segment::DHT)(input),
                0xDA => {
                    let (input, sos) = parse_sos(input)?;
                    let (input, data) = take_until_marker(input)?;
                    Ok((input, Segment::SOS(sos)))
                }
                0xD9 => Ok((input, Segment::End)),
                _ => {
                    let (input, length) = be_u16(input)?;
                    let (input, _) = take(length as usize - 2)(input)?;
                    parse_segment(input)
                }
            }
        }
        _ => Ok((input, Segment::Data(vec![marker]))),
    }
}

fn take_until_marker(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut i = 0;
    while i < input.len() - 1 {
        if input[i] == 0xFF && input[i + 1] != 0x00 {
            return Ok((&input[i..], input[..i].to_vec()));
        }
        i += 1;
    }
    Ok((&[], input.to_vec()))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegImage> {
    let (input, segments) = many0(parse_segment)(input)?;
    Ok((input, JpegImage { segments }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <jpeg_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_jpeg(&buffer) {
        Ok((_, jpeg)) => println!("{:#?}", jpeg),
        Err(e) => eprintln!("Error parsing JPEG: {:?}", e),
    }
}