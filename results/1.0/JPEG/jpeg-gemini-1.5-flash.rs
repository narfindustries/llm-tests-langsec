use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    error::VerboseError,
    multi::count,
    number::complete::{be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};

#[derive(Debug)]
struct JpegHeader {
    soi: [u8; 2],
    app0: Option<App0>,
    dqt: Vec<Dqt>,
    sof: Sof,
    dht: Vec<Dht>,
    sos: Sos,
    eoi: [u8; 2],
}

#[derive(Debug)]
struct App0 {
    marker: [u8; 2],
    length: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct Dqt {
    marker: [u8; 2],
    length: u16,
    data: Vec<u8>,
}


#[derive(Debug)]
struct Sof {
    marker: [u8; 2],
    length: u16,
    precision: u8,
    height: u16,
    width: u16,
    num_components: u8,
    components: Vec<Component>,

}

#[derive(Debug)]
struct Component {
    id: u8,
    h_sampling_factor: u8,
    v_sampling_factor: u8,
    quant_table_id: u8,
}

#[derive(Debug)]
struct Dht {
    marker: [u8; 2],
    length: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct Sos {
    marker: [u8; 2],
    length: u16,
    data: Vec<u8>,
}


fn app0(input: &[u8]) -> IResult<&[u8], App0, VerboseError<&[u8]>> {
    let (input, marker) = tag(b"\xFF\xE0")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take((length as usize) - 2)(input)?;
    Ok((input, App0 { marker, length, data: data.to_vec() }))
}

fn dqt(input: &[u8]) -> IResult<&[u8], Dqt, VerboseError<&[u8]>> {
    let (input, marker) = tag(b"\xFF\xDB")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take((length as usize) - 2)(input)?;
    Ok((input, Dqt { marker, length, data: data.to_vec() }))
}

fn sof(input: &[u8]) -> IResult<&[u8], Sof, VerboseError<&[u8]>> {
    let (input, marker) = tag(b"\xFF\xC0")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, precision) = take(1usize)(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, num_components) = take(1usize)(input)?;
    let (input, components) = count(component, num_components[0] as usize)(input)?;
    Ok((input, Sof { marker: marker.try_into().unwrap(), length, precision: precision[0], height, width, num_components: num_components[0], components }))
}

fn component(input: &[u8]) -> IResult<&[u8], Component, VerboseError<&[u8]>> {
    let (input, id) = take(1usize)(input)?;
    let (input, h_sampling_factor) = take(1usize)(input)?;
    let (input, v_sampling_factor) = take(1usize)(input)?;
    let (input, quant_table_id) = take(1usize)(input)?;

    Ok((input, Component { id: id[0], h_sampling_factor: h_sampling_factor[0], v_sampling_factor: v_sampling_factor[0], quant_table_id: quant_table_id[0] }))

}


fn dht(input: &[u8]) -> IResult<&[u8], Dht, VerboseError<&[u8]>> {
    let (input, marker) = tag(b"\xFF\xC4")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take((length as usize) - 2)(input)?;
    Ok((input, Dht { marker, length, data: data.to_vec() }))
}

fn sos(input: &[u8]) -> IResult<&[u8], Sos, VerboseError<&[u8]>> {
    let (input, marker) = tag(b"\xFF\xDA")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take((length as usize) - 2)(input)?;
    Ok((input, Sos { marker, length, data: data.to_vec() }))
}

fn jpeg(input: &[u8]) -> IResult<&[u8], JpegHeader, VerboseError<&[u8]>> {
    let (input, soi) = tag(b"\xFF\xD8")(input)?;
    let (input, app0) = opt(app0)(input)?;
    let (input, dqt) = many0(dqt)(input)?;
    let (input, sof) = sof(input)?;
    let (input, dht) = many0(dht)(input)?;
    let (input, sos) = sos(input)?;
    let (input, eoi) = tag(b"\xFF\xD9")(input)?;

    Ok((
        input,
        JpegHeader {
            soi: soi.try_into().unwrap(),
            app0,
            dqt,
            sof,
            dht,
            sos,
            eoi: eoi.try_into().unwrap(),
        },
    ))
}

fn many0<I, O, E, F>(f: F) -> impl Fn(I) -> IResult<I, Vec<O>, E>
where
    F: Fn(I) -> IResult<I, O, E>,
    I: Clone,
{
    nom::multi::many0(f)
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: jpeg_parser <input_file>");
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match jpeg(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => println!("Error parsing JPEG: {:?}", e),
    }
}
