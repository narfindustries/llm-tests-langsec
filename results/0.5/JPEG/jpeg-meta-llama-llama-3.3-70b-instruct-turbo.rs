use nom::{
    bytes::complete::{tag, take},
    combinator::{opt},
    multi::{length_data, many_till},
    number::complete::{be_u16, be_u8},
    sequence::{tuple},
    IResult, Err,
};
use std::env;
use std::fs::File;
use std::io::{Read, stdin, Error, ErrorKind};

#[derive(Debug, PartialEq)]
enum Marker {
    SOI,
    APP0,
    APP1,
    DQT,
    DHT,
    SOF0,
    SOS,
    EOI,
}

fn marker(input: &[u8]) -> IResult<&[u8], Marker> {
    let (input, marker) = take(2u8)(input)?;
    match marker {
        [0xFF, 0xD8] => Ok((input, Marker::SOI)),
        [0xFF, 0xE0] => Ok((input, Marker::APP0)),
        [0xFF, 0xE1] => Ok((input, Marker::APP1)),
        [0xFF, 0xDB] => Ok((input, Marker::DQT)),
        [0xFF, 0xC4] => Ok((input, Marker::DHT)),
        [0xFF, 0xC0] => Ok((input, Marker::SOF0)),
        [0xFF, 0xDA] => Ok((input, Marker::SOS)),
        [0xFF, 0xD9] => Ok((input, Marker::EOI)),
        _ => Err(Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::AlphaNumeric))),
    }
}

#[derive(Debug, PartialEq)]
struct APP0 {
    length: u16,
    identifier: String,
    version: String,
    units: u8,
    xdensity: u16,
    ydensity: u16,
    thumb_width: u8,
    thumb_height: u8,
}

fn app0(input: &[u8]) -> IResult<&[u8], APP0> {
    let (input, _) = tag([0xFF, 0xE0])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5u8)(input)?;
    let identifier = String::from_utf8_lossy(identifier).into_owned();
    let (input, version) = take(2u8)(input)?;
    let version = String::from_utf8_lossy(version).into_owned();
    let (input, units) = be_u8(input)?;
    let (input, xdensity) = be_u16(input)?;
    let (input, ydensity) = be_u16(input)?;
    let (input, thumb_width) = be_u8(input)?;
    let (input, thumb_height) = be_u8(input)?;
    Ok((
        &input[length as usize - 18..],
        APP0 {
            length,
            identifier,
            version,
            units,
            xdensity,
            ydensity,
            thumb_width,
            thumb_height,
        },
    ))
}

#[derive(Debug, PartialEq)]
struct APP1 {
    length: u16,
    identifier: String,
}

fn app1(input: &[u8]) -> IResult<&[u8], APP1> {
    let (input, _) = tag([0xFF, 0xE1])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(4u8)(input)?;
    let identifier = String::from_utf8_lossy(identifier).into_owned();
    Ok((
        &input[length as usize - 6..],
        APP1 { length, identifier },
    ))
}

#[derive(Debug, PartialEq)]
struct DQT {
    length: u16,
    table_precision: u8,
    table_identifier: u8,
    quantization_table: Vec<u8>,
}

fn dqt(input: &[u8]) -> IResult<&[u8], DQT> {
    let (input, _) = tag([0xFF, 0xDB])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, table_precision) = be_u8(input)?;
    let (input, table_identifier) = be_u8(input)?;
    let (input, quantization_table) = length_data(be_u16)(input)?;
    Ok((
        &input[length as usize - 67..],
        DQT {
            length,
            table_precision,
            table_identifier,
            quantization_table: quantization_table.to_vec(),
        },
    ))
}

#[derive(Debug, PartialEq)]
struct DHT {
    length: u16,
    table_class: u8,
    table_identifier: u8,
    number_of_codes: u8,
    huffman_codes: Vec<u8>,
}

fn dht(input: &[u8]) -> IResult<&[u8], DHT> {
    let (input, _) = tag([0xFF, 0xC4])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, table_class) = be_u8(input)?;
    let (input, table_identifier) = be_u8(input)?;
    let (input, number_of_codes) = be_u8(input)?;
    let (input, huffman_codes) = length_data(be_u16)(input)?;
    Ok((
        &input[length as usize - 19..],
        DHT {
            length,
            table_class,
            table_identifier,
            number_of_codes,
            huffman_codes: huffman_codes.to_vec(),
        },
    ))
}

#[derive(Debug, PartialEq)]
struct SOF0 {
    length: u16,
    precision: u8,
    image_height: u16,
    image_width: u16,
    number_of_components: u8,
    components: Vec<(u8, u8, u8)>,
}

fn sof0(input: &[u8]) -> IResult<&[u8], SOF0> {
    let (input, _) = tag([0xFF, 0xC0])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, image_height) = be_u16(input)?;
    let (input, image_width) = be_u16(input)?;
    let (input, number_of_components) = be_u8(input)?;
    let (input, components) = many_till(
        tuple((be_u8, be_u8, be_u8)),
        tag([0x00]),
    )(input)?;
    Ok((
        &input[length as usize - 11 - (number_of_components as usize * 3)..],
        SOF0 {
            length,
            precision,
            image_height,
            image_width,
            number_of_components,
            components: components.0,
        },
    ))
}

#[derive(Debug, PartialEq)]
struct SOS {
    length: u16,
    number_of_components: u8,
    components: Vec<(u8, u8, u8)>,
}

fn sos(input: &[u8]) -> IResult<&[u8], SOS> {
    let (input, _) = tag([0xFF, 0xDA])(input)?;
    let (input, length) = be_u16(input)?;
    let (input, number_of_components) = be_u8(input)?;
    let (input, components) = many_till(
        tuple((be_u8, be_u8, be_u8)),
        tag([0x00]),
    )(input)?;
    Ok((
        &input[length as usize - 6 - (number_of_components as usize * 3)..],
        SOS {
            length,
            number_of_components,
            components: components.0,
        },
    ))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let mut file: Box<dyn Read> = if args.len() > 1 {
        Box::new(File::open(&args[1])?)
    } else {
        Box::new(stdin())
    };
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    let (input, _) = marker(&data).map_err(|e| Error::new(ErrorKind::InvalidData, e.to_string()))?;
    let (_input, app0) = opt(app0)(input).map_err(|e| Error::new(ErrorKind::InvalidData, e.to_string()))?;
    let (_input, app1) = opt(app1)(input).map_err(|e| Error::new(ErrorKind::InvalidData, e.to_string()))?;
    let (_input, dqt) = opt(dqt)(input).map_err(|e| Error::new(ErrorKind::InvalidData, e.to_string()))?;
    let (_input, dht) = opt(dht)(input).map_err(|e| Error::new(ErrorKind::InvalidData, e.to_string()))?;
    let (_input, sof0) = sof0(input).map_err(|e| Error::new(ErrorKind::InvalidData, e.to_string()))?;
    let (_input, sos) = sos(input).map_err(|e| Error::new(ErrorKind::InvalidData, e.to_string()))?;
    println!("{:?}", app0);
    println!("{:?}", app1);
    println!("{:?}", dqt);
    println!("{:?}", dht);
    println!("{:?}", sof0);
    println!("{:?}", sos);
    Ok(())
}