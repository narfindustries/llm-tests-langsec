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
    num_components: u8,
}

#[derive(Debug)]
struct Component {
    id: u8,
    sampling_factor: u8,
    quant_table_id: u8,
}

#[derive(Debug)]
struct JPEG {
    soi: (),
    app0: Option<()>,
    sof0: SOF0,
    components: Vec<Component>,
    sos: (),
    eoi: (),
}

fn parse_sof0(input: &[u8]) -> IResult<&[u8], SOF0> {
    let (input, _) = tag(b"\xFF\xC0")(input)?;
    let (input, _length) = be_u16(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, num_components) = be_u8(input)?;
    let mut components = Vec::new();
    let mut remaining = input;
    for _ in 0..num_components {
        let (input, component) = parse_component(remaining)?;
        remaining = input;
        components.push(component);
    }
    Ok((remaining, SOF0 { precision, height, width, num_components }))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, id) = be_u8(input)?;
    let (input, sampling_factor) = be_u8(input)?;
    let (input, quant_table_id) = be_u8(input)?;
    Ok((input, Component { id, sampling_factor, quant_table_id }))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JPEG> {
    let (input, _) = tag(b"\xFF\xD8")(input)?;
    let (input, app0) = parse_app0(input).ok().map_or((input, None), |(i, _)| (i, Some(())));
    let (input, sof0) = parse_sof0(input)?;
    let (input, components) = take(sof0.num_components as usize * 3)(input)?;
    let (input, _) = tag(b"\xFF\xDA")(input)?;
    let (input, _) = tag(b"\xFF\xD9")(input)?;
    Ok((input, JPEG {
        soi: (),
        app0,
        sof0,
        components: components.chunks(3).map(|c| Component {
            id: c[0],
            sampling_factor: c[1],
            quant_table_id: c[2],
        }).collect(),
        sos: (),
        eoi: (),
    }))
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(b"\xFF\xE0")(input)?;
    let (input, _length) = be_u16(input)?;
    let (input, _) = take(14usize)(input)?;
    Ok((input, ()))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <JPEG_FILE>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_jpeg(&buffer) {
        Ok((_, jpeg)) => println!("{:?}", jpeg),
        Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
    }

    Ok(())
}