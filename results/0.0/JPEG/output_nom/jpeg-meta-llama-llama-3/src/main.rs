use nom::{
    bytes::complete::{tag, take},
    error::ParseError,
    multi::{many1},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, BufReader};

#[derive(Debug, PartialEq)]
enum Marker {
    SOI,
    EOI,
    SOF0,
    SOF1,
    SOF2,
    SOF3,
    DHT,
    DAC,
    DNL,
    DRI,
    DQT,
    Other(u8),
}

impl Marker {
    fn parse(input: &[u8]) -> IResult<&[u8], Marker> {
        let (input, marker) = take(2u8)(input)?;
        match marker {
            [0xFF, 0xD8] => Ok((input, Marker::SOI)),
            [0xFF, 0xD9] => Ok((input, Marker::EOI)),
            [0xFF, 0xC0] => Ok((input, Marker::SOF0)),
            [0xFF, 0xC1] => Ok((input, Marker::SOF1)),
            [0xFF, 0xC2] => Ok((input, Marker::SOF2)),
            [0xFF, 0xC3] => Ok((input, Marker::SOF3)),
            [0xFF, 0xC4] => Ok((input, Marker::DHT)),
            [0xFF, 0xCC] => Ok((input, Marker::DAC)),
            [0xFF, 0xCD] => Ok((input, Marker::DNL)),
            [0xFF, 0xCE] => Ok((input, Marker::DRI)),
            [0xFF, 0xCF] => Ok((input, Marker::DQT)),
            [0xFF, other] => Ok((input, Marker::Other(*other))),
            _ => Err(nom::Err::Error(nom::error::Error::from_error_kind(input, nom::error::ErrorKind::Tag))),
        }
    }
}

#[derive(Debug, PartialEq)]
struct SOF0 {
    precision: u8,
    height: u16,
    width: u16,
    num_components: u8,
    components: Vec<Component>,
}

#[derive(Debug, PartialEq)]
struct Component {
    id: u8,
    horizontal_sampling_factor: u8,
    vertical_sampling_factor: u8,
    quantization_table_number: u8,
}

impl SOF0 {
    fn parse(input: &[u8]) -> IResult<&[u8], SOF0> {
        let (input, _) = tag([0xFF, 0xC0])(input)?;
        let (_input, _length) = be_u16(input)?;
        let (input, precision) = be_u8(input)?;
        let (input, height) = be_u16(input)?;
        let (input, width) = be_u16(input)?;
        let (input, num_components) = be_u8(input)?;
        let (input, components) = many1(Component::parse)(input)?;
        Ok((input, SOF0 {
            precision,
            height,
            width,
            num_components,
            components,
        }))
    }
}

impl Component {
    fn parse(input: &[u8]) -> IResult<&[u8], Component> {
        let (input, id) = be_u8(input)?;
        let (input, horizontal_sampling_factor) = be_u8(input)?;
        let (input, vertical_sampling_factor) = be_u8(input)?;
        let (input, quantization_table_number) = be_u8(input)?;
        Ok((input, Component {
            id,
            horizontal_sampling_factor,
            vertical_sampling_factor,
            quantization_table_number,
        }))
    }
}

#[derive(Debug, PartialEq)]
struct DHT {
    table_class: u8,
    table_destination: u8,
    num_codes: u8,
    code_lengths: Vec<u8>,
    code_values: Vec<u8>,
}

impl DHT {
    fn parse(input: &[u8]) -> IResult<&[u8], DHT> {
        let (input, _) = tag([0xFF, 0xC4])(input)?;
        let (_input, _length) = be_u16(input)?;
        let (input, table_class) = be_u8(input)?;
        let (input, table_destination) = be_u8(input)?;
        let (input, num_codes) = be_u8(input)?;
        let (input, code_lengths) = many1(be_u8)(input)?;
        let (input, code_values) = many1(be_u8)(input)?;
        Ok((input, DHT {
            table_class,
            table_destination,
            num_codes,
            code_lengths,
            code_values,
        }))
    }
}

#[derive(Debug, PartialEq)]
struct DQT {
    table_precision: u8,
    table_number: u8,
    quantization_values: Vec<u8>,
}

impl DQT {
    fn parse(input: &[u8]) -> IResult<&[u8], DQT> {
        let (input, _) = tag([0xFF, 0xCF])(input)?;
        let (_input, _length) = be_u16(input)?;
        let (input, table_precision) = be_u8(input)?;
        let (input, table_number) = be_u8(input)?;
        let (input, quantization_values) = many1(be_u8)(input)?;
        Ok((input, DQT {
            table_precision,
            table_number,
            quantization_values,
        }))
    }
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
    let (input, marker) = Marker::parse(&data).unwrap();
    match marker {
        Marker::SOI => {
            let (_input, sof0) = SOF0::parse(input).unwrap();
            println!("{:?}", sof0);
        }
        Marker::DHT => {
            let (_input, dht) = DHT::parse(input).unwrap();
            println!("{:?}", dht);
        }
        Marker::DQT => {
            let (_input, dqt) = DQT::parse(input).unwrap();
            println!("{:?}", dqt);
        }
        _ => println!("Unsupported marker"),
    }
}