use nom::{
    bytes::complete::{take},
    multi::{many1},
    number::complete::{be_u16, be_u8},
    IResult,
};
use nom::error::{Error, ErrorKind};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum Marker {
    SOI,
    APP0,
    APP1,
    DQT,
    DHT,
    SOF0,
    SOF2,
    DRI,
    SOS,
    RST(u8),
    EOI,
}

#[derive(Debug, PartialEq)]
struct APP0 {
    length: u16,
    identifier: [u8; 5],
    version: u16,
    units: u8,
    xdensity: u16,
    ydensity: u16,
    xthumbnail: u8,
    ythumbnail: u8,
}

#[derive(Debug, PartialEq)]
struct APP1 {
    length: u16,
    identifier: [u8; 5],
    exif_data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct DQT {
    length: u16,
    table_number: u8,
    precision: u8,
    table_data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct DHT {
    length: u16,
    table_class: u8,
    table_number: u8,
    table_data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct SOF0 {
    length: u16,
    precision: u8,
    image_height: u16,
    image_width: u16,
    number_of_components: u8,
    components: Vec<Component>,
}

#[derive(Debug, PartialEq)]
struct Component {
    id: u8,
    horizontal_sampling_factor: u8,
    vertical_sampling_factor: u8,
    number_of_bits_per_component: u8,
}

#[derive(Debug, PartialEq)]
struct SOF2 {
    length: u16,
    precision: u8,
    image_height: u16,
    image_width: u16,
    number_of_components: u8,
    components: Vec<Component>,
}

#[derive(Debug, PartialEq)]
struct DRI {
    length: u16,
    restart_interval: u16,
}

#[derive(Debug, PartialEq)]
struct SOS {
    length: u16,
    number_of_components: u8,
    components: Vec<Component>,
}

fn marker(input: &[u8]) -> IResult<&[u8], Marker> {
    let (input, marker) = take(2usize)(input)?;
    match marker {
        [0xFF, 0xD8] => Ok((input, Marker::SOI)),
        [0xFF, 0xE0] => Ok((input, Marker::APP0)),
        [0xFF, 0xE1] => Ok((input, Marker::APP1)),
        [0xFF, 0xDB] => Ok((input, Marker::DQT)),
        [0xFF, 0xC4] => Ok((input, Marker::DHT)),
        [0xFF, 0xC0] => Ok((input, Marker::SOF0)),
        [0xFF, 0xC2] => Ok((input, Marker::SOF2)),
        [0xFF, 0xDD] => Ok((input, Marker::DRI)),
        [0xFF, 0xDA] => Ok((input, Marker::SOS)),
        [0xFF, 0xD0..=0xFF, 0xD7] => Ok((input, Marker::RST(marker[1]))),
        [0xFF, 0xD9] => Ok((input, Marker::EOI)),
        _ => Err(nom::Err::Error(Error::new(input, ErrorKind::AlphaNumeric))),
    }
}

fn app0(input: &[u8]) -> IResult<&[u8], APP0> {
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;
    let (input, version) = be_u16(input)?;
    let (input, units) = be_u8(input)?;
    let (input, xdensity) = be_u16(input)?;
    let (input, ydensity) = be_u16(input)?;
    let (input, xthumbnail) = be_u8(input)?;
    let (input, ythumbnail) = be_u8(input)?;
    Ok((
        input,
        APP0 {
            length,
            identifier: identifier.try_into().unwrap(),
            version,
            units,
            xdensity,
            ydensity,
            xthumbnail,
            ythumbnail,
        },
    ))
}

fn app1(input: &[u8]) -> IResult<&[u8], APP1> {
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;
    let (input, exif_data) = take(length as usize - 5)(input)?;
    Ok((
        input,
        APP1 {
            length,
            identifier: identifier.try_into().unwrap(),
            exif_data: exif_data.to_vec(),
        },
    ))
}

fn dqt(input: &[u8]) -> IResult<&[u8], DQT> {
    let (input, length) = be_u16(input)?;
    let (input, table_number) = be_u8(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, table_data) = take(64usize)(input)?;
    Ok((
        input,
        DQT {
            length,
            table_number,
            precision,
            table_data: table_data.to_vec(),
        },
    ))
}

fn dht(input: &[u8]) -> IResult<&[u8], DHT> {
    let (input, length) = be_u16(input)?;
    let (input, table_class) = be_u8(input)?;
    let (input, table_number) = be_u8(input)?;
    let (input, table_data) = take(length as usize - 3)(input)?;
    Ok((
        input,
        DHT {
            length,
            table_class,
            table_number,
            table_data: table_data.to_vec(),
        },
    ))
}

fn sof0(input: &[u8]) -> IResult<&[u8], SOF0> {
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, image_height) = be_u16(input)?;
    let (input, image_width) = be_u16(input)?;
    let (input, number_of_components) = be_u8(input)?;
    let (input, components) = many1(component)(input)?;
    Ok((
        input,
        SOF0 {
            length,
            precision,
            image_height,
            image_width,
            number_of_components,
            components,
        },
    ))
}

fn sof2(input: &[u8]) -> IResult<&[u8], SOF2> {
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, image_height) = be_u16(input)?;
    let (input, image_width) = be_u16(input)?;
    let (input, number_of_components) = be_u8(input)?;
    let (input, components) = many1(component)(input)?;
    Ok((
        input,
        SOF2 {
            length,
            precision,
            image_height,
            image_width,
            number_of_components,
            components,
        },
    ))
}

fn dri(input: &[u8]) -> IResult<&[u8], DRI> {
    let (input, length) = be_u16(input)?;
    let (input, restart_interval) = be_u16(input)?;
    Ok((input, DRI { length, restart_interval }))
}

fn sos(input: &[u8]) -> IResult<&[u8], SOS> {
    let (input, length) = be_u16(input)?;
    let (input, number_of_components) = be_u8(input)?;
    let (input, components) = many1(component)(input)?;
    Ok((
        input,
        SOS {
            length,
            number_of_components,
            components,
        },
    ))
}

fn component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, id) = be_u8(input)?;
    let (input, horizontal_sampling_factor) = be_u8(input)?;
    let (input, vertical_sampling_factor) = be_u8(input)?;
    let (input, number_of_bits_per_component) = be_u8(input)?;
    Ok((
        input,
        Component {
            id,
            horizontal_sampling_factor,
            vertical_sampling_factor,
            number_of_bits_per_component,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();
    let (input, marker) = marker(&data).unwrap();
    match marker {
        Marker::SOI => {
            let (_input, app0) = app0(input).unwrap();
            println!("{:?}", app0);
        }
        Marker::APP0 => {
            let (_input, app0) = app0(input).unwrap();
            println!("{:?}", app0);
        }
        Marker::APP1 => {
            let (_input, app1) = app1(input).unwrap();
            println!("{:?}", app1);
        }
        Marker::DQT => {
            let (_input, dqt) = dqt(input).unwrap();
            println!("{:?}", dqt);
        }
        Marker::DHT => {
            let (_input, dht) = dht(input).unwrap();
            println!("{:?}", dht);
        }
        Marker::SOF0 => {
            let (_input, sof0) = sof0(input).unwrap();
            println!("{:?}", sof0);
        }
        Marker::SOF2 => {
            let (_input, sof2) = sof2(input).unwrap();
            println!("{:?}", sof2);
        }
        Marker::DRI => {
            let (_input, dri) = dri(input).unwrap();
            println!("{:?}", dri);
        }
        Marker::SOS => {
            let (_input, sos) = sos(input).unwrap();
            println!("{:?}", sos);
        }
        Marker::RST(_) => {
            println!("RST marker");
        }
        Marker::EOI => {
            println!("EOI marker");
        }
    }
}