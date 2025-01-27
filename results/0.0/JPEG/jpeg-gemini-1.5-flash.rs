use std::env;
use std::fs;
use std::error::Error;
use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::tuple,
    multi::count,
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
    identifier: [u8; 5],
    length: u16,
    // Add other APP0 fields as needed
}

#[derive(Debug)]
struct Dqt {
    length: u16,
    // Add quantization table data
}

#[derive(Debug)]
struct Sof {
    length: u16,
    // Add other SOF fields as needed
}

#[derive(Debug)]
struct Dht {
    length: u16,
    // Add Huffman table data
}

#[derive(Debug)]
struct Sos {
    length: u16,
    // Add other SOS fields as needed
}


fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegHeader> {
    let (input, soi) = tag(b"\xFF\xD8")(input)?;
    let (input, app0) = parse_app0(input)?;
    let (input, dqt) = parse_dqt(input)?;
    let (input, sof) = parse_sof(input)?;
    let (input, dht) = parse_dht(input)?;
    let (input, sos) = parse_sos(input)?;
    let (input, eoi) = tag(b"\xFF\xD9")(input)?;

    Ok((input, JpegHeader { soi, app0, dqt, sof, dht, sos, eoi }))
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], Option<App0>> {
    let (input, marker) = tag(b"\xFF\xE0")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;
    let (input, _) = take(length as usize - 2)(input)?; // Consume remaining data

    Ok((input, Some(App0 { identifier: identifier.try_into().unwrap(), length })))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], Vec<Dqt>> {
    let mut dqts = Vec::new();
    let mut input = input;
    loop {
        match tag(b"\xFF\xDB")(input) {
            Ok((i, _)) => {
                let (i, length) = be_u16(i)?;
                let (i, _) = take(length as usize - 2)(i)?; // Consume quantization table data
                dqts.push(Dqt { length });
                input = i;
            }
            Err(_) => break,
        }
    }
    Ok((input, dqts))
}

fn parse_sof(input: &[u8]) -> IResult<&[u8], Sof> {
    let (input, _) = tag(b"\xFF\xC0")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, _) = take(length as usize - 2)(input)?; // Consume SOF data
    Ok((input, Sof { length }))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], Vec<Dht>> {
    let mut dhts = Vec::new();
    let mut input = input;
    loop {
        match tag(b"\xFF\xC4")(input) {
            Ok((i, _)) => {
                let (i, length) = be_u16(i)?;
                let (i, _) = take(length as usize - 2)(i)?; // Consume Huffman table data
                dhts.push(Dht { length });
                input = i;
            }
            Err(_) => break,
        }
    }
    Ok((input, dhts))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], Sos> {
    let (input, _) = tag(b"\xFF\xDA")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, _) = take(length as usize - 2)(input)?; // Consume SOS data
    Ok((input, Sos { length }))
}


fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <jpeg_file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let contents = fs::read(filename)?;
    match parse_jpeg(&contents) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Error parsing JPEG: {}", e),
    }
    Ok(())
}
