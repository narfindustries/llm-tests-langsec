use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct SOF0 {
    precision: u8,
    height: u16,
    width: u16,
    number_of_components: u8,
}

#[derive(Debug)]
struct Component {
    component_id: u8,
    sampling_factor: u8,
    quantization_table_id: u8,
}

#[derive(Debug)]
struct JPEG {
    soi: (),
    app0: Vec<u8>,
    dqt: Vec<u8>,
    sof0: SOF0,
    components: Vec<Component>,
    sos: Vec<u8>,
    eoi: (),
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], SOF0> {
    let (input, _) = tag([0xFF, 0xC0])(input)?;
    let (input, _length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, number_of_components) = be_u8(input)?;
    let (input, components) = nom::multi::count(parse_component, number_of_components as usize)(input)?;

    Ok((input, SOF0 { precision, height, width, number_of_components }))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, component_id) = be_u8(input)?;
    let (input, sampling_factor) = be_u8(input)?;
    let (input, quantization_table_id) = be_u8(input)?;
    Ok((input, Component { component_id, sampling_factor, quantization_table_id }))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JPEG> {
    let (input, _) = tag([0xFF, 0xD8])(input)?;
    let (input, app0) = take_until_tag(0xFF, 0xE0)(input)?;
    let (input, dqt) = take_until_tag(0xFF, 0xDB)(input)?;
    let (input, sof0) = parse_sof0(input)?;
    let (input, components) = nom::multi::count(parse_component, sof0.number_of_components as usize)(input)?;
    let (input, sos) = take_until_tag(0xFF, 0xDA)(input)?;
    let (input, _) = tag([0xFF, 0xD9])(input)?;

    Ok((input, JPEG { soi: (), app0: app0.to_vec(), dqt: dqt.to_vec(), sof0, components, sos: sos.to_vec(), eoi: () }))
}

fn take_until_tag<'a>(tag1: u8, tag2: u8) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], &'a [u8]> {
    move |input: &[u8]| {
        let mut index = 0;
        while index + 1 < input.len() {
            if input[index] == tag1 && input[index + 1] == tag2 {
                return Ok((&input[index..], &input[..index]));
            }
            index += 1;
        }
        Err(nom::Err::Error((input, nom::error::ErrorKind::Tag)))
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No file provided"));
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_jpeg(&buffer) {
        Ok((_, jpeg)) => println!("{:?}", jpeg),
        Err(e) => println!("Failed to parse JPEG: {:?}", e),
    }

    Ok(())
}