use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{many0, many1},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
enum JpegMarker {
    SOI,
    APP0,
    APP1,
    APP2,
    APP3,
    APP4,
    APP5,
    APP6,
    APP7,
    APP8,
    APP9,
    APP10,
    APP11,
    APP12,
    APP13,
    APP14,
    APP15,
    DHT,
    DQT,
    SOF0,
    SOF1,
    SOF2,
    SOF3,
    DRI,
    SOS,
    EOI,
    Other(u8),
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    map_res(be_u8, |marker| match marker {
        0xD8 => Ok(JpegMarker::SOI),
        0xE0 => Ok(JpegMarker::APP0),
        0xE1 => Ok(JpegMarker::APP1),
        0xE2 => Ok(JpegMarker::APP2),
        0xE3 => Ok(JpegMarker::APP3),
        0xE4 => Ok(JpegMarker::APP4),
        0xE5 => Ok(JpegMarker::APP5),
        0xE6 => Ok(JpegMarker::APP6),
        0xE7 => Ok(JpegMarker::APP7),
        0xE8 => Ok(JpegMarker::APP8),
        0xE9 => Ok(JpegMarker::APP9),
        0xEA => Ok(JpegMarker::APP10),
        0xEB => Ok(JpegMarker::APP11),
        0xEC => Ok(JpegMarker::APP12),
        0xED => Ok(JpegMarker::APP13),
        0xEE => Ok(JpegMarker::APP14),
        0xEF => Ok(JpegMarker::APP15),
        0xC4 => Ok(JpegMarker::DHT),
        0xDB => Ok(JpegMarker::DQT),
        0xC0 => Ok(JpegMarker::SOF0),
        0xC1 => Ok(JpegMarker::SOF1),
        0xC2 => Ok(JpegMarker::SOF2),
        0xC3 => Ok(JpegMarker::SOF3),
        0xDD => Ok(JpegMarker::DRI),
        0xDA => Ok(JpegMarker::SOS),
        0xD9 => Ok(JpegMarker::EOI),
        _ => Ok(JpegMarker::Other(marker)),
    })(input)
}

#[derive(Debug)]
struct App0 {
    length: u16,
    identifier: String,
    version: String,
    units: u8,
    x_density: u16,
    y_density: u16,
    thumbnail_x: u8,
    thumbnail_y: u8,
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], App0> {
    let (input, _) = tag([0xFF, 0xE0])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5u8)(input)?;
    let (input, version) = take(2u8)(input)?;
    let (input, units) = be_u8(input)?;
    let (input, x_density) = be_u16(input)?;
    let (input, y_density) = be_u16(input)?;
    let (input, thumbnail_x) = be_u8(input)?;
    let (input, thumbnail_y) = be_u8(input)?;
    let app0 = App0 {
        length,
        identifier: String::from_utf8_lossy(identifier).into_owned(),
        version: String::from_utf8_lossy(version).into_owned(),
        units,
        x_density,
        y_density,
        thumbnail_x,
        thumbnail_y,
    };
    Ok((input, app0))
}

#[derive(Debug)]
struct Dht {
    length: u16,
    table_class: u8,
    table_id: u8,
    table: Vec<u8>,
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], Dht> {
    let (input, _) = tag([0xFF, 0xC4])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, table_class) = be_u8(input)?;
    let (input, table_id) = be_u8(input)?;
    let (input, table) = take(length - 2)(input)?;
    let dht = Dht {
        length,
        table_class,
        table_id,
        table: table.to_vec(),
    };
    Ok((input, dht))
}

#[derive(Debug)]
struct Dqt {
    length: u16,
    precision: u8,
    table_id: u8,
    table: Vec<u8>,
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], Dqt> {
    let (input, _) = tag([0xFF, 0xDB])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, table_id) = be_u8(input)?;
    let (input, table) = take(length - 2)(input)?;
    let dqt = Dqt {
        length,
        precision,
        table_id,
        table: table.to_vec(),
    };
    Ok((input, dqt))
}

#[derive(Debug)]
struct Sof0 {
    length: u16,
    precision: u8,
    height: u16,
    width: u16,
    num_components: u8,
    components: Vec<(u8, u8, u8)>,
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], Sof0> {
    let (input, _) = tag([0xFF, 0xC0])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, components) = many1(map(
        take(3u8),
        |component: &[u8]| (component[0], component[1], component[2]),
    ))(input)?;
    let sof0 = Sof0 {
        length,
        precision,
        height,
        width,
        num_components,
        components: components.to_vec(),
    };
    Ok((input, sof0))
}

#[derive(Debug)]
struct Sos {
    length: u16,
    num_components: u8,
    components: Vec<(u8, u8)>,
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], Sos> {
    let (input, _) = tag([0xFF, 0xDA])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let (input, components) = many1(map(
        take(2u8),
        |component: &[u8]| (component[0], component[1]),
    ))(input)?;
    let sos = Sos {
        length,
        num_components,
        components: components.to_vec(),
    };
    Ok((input, sos))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut data = Vec::new();
    reader.read_to_end(&mut data).unwrap();
    let (input, _) = many0(map(
        parse_marker,
        |marker| match marker {
            JpegMarker::APP0 => {
                let (input, app0) = parse_app0(input)?;
                println!("{:?}", app0);
                input
            }
            JpegMarker::DHT => {
                let (input, dht) = parse_dht(input)?;
                println!("{:?}", dht);
                input
            }
            JpegMarker::DQT => {
                let (input, dqt) = parse_dqt(input)?;
                println!("{:?}", dqt);
                input
            }
            JpegMarker::SOF0 => {
                let (input, sof0) = parse_sof0(input)?;
                println!("{:?}", sof0);
                input
            }
            JpegMarker::SOS => {
                let (input, sos) = parse_sos(input)?;
                println!("{:?}", sos);
                input
            }
            _ => input,
        },
    ))(&data);
}