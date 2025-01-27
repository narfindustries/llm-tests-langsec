use std::env;
use std::fs;
use std::error::Error;
use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16, be_u32},
    multi::count,
    sequence::tuple,
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
    identifier: [u8; 2],
    length: u16,
    // Add quantization table data
}

#[derive(Debug)]
struct Sof {
    identifier: [u8; 2],
    length: u16,
    // Add other SOF fields as needed
}

#[derive(Debug)]
struct Dht {
    identifier: [u8; 2],
    length: u16,
    // Add Huffman table data
}

#[derive(Debug)]
struct Sos {
    identifier: [u8; 2],
    length: u16,
    // Add other SOS fields as needed
}


fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegHeader> {
    let (input, soi) = tag(b"\xFF\xD8")(input)?;
    let (input, app0) = parse_optional_app0(input)?;
    let (input, dqt) = parse_dqt_segments(input)?;
    let (input, sof) = parse_sof(input)?;
    let (input, dht) = parse_dht_segments(input)?;
    let (input, sos) = parse_sos(input)?;
    let (input, eoi) = tag(b"\xFF\xD9")(input)?;

    Ok((input, JpegHeader { soi, app0, dqt, sof, dht, sos, eoi }))
}

fn parse_optional_app0(input: &[u8]) -> IResult<&[u8], Option<App0>> {
    let (input, res) = nom::branch::alt((
        parse_app0,
        nom::combinator::value(None, nom::combinator::rest)
    ))(input)?;
    Ok((input, res))
}

fn parse_app0(input: &[u8]) -> IResult<&[u8], App0> {
    let (input, marker) = tag(b"\xFF\xE0")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, identifier) = take(5usize)(input)?;
    // Parse the rest of the APP0 segment based on length
    let (input, _) = take(length as usize - 2)(input)?;
    Ok((input, App0 { identifier: *identifier.try_into().unwrap(), length }))
}

fn parse_dqt_segments(input: &[u8]) -> IResult<&[u8], Vec<Dqt>> {
    let (input, segments) = nom::multi::many0(parse_dqt)(input)?;
    Ok((input, segments))
}

fn parse_dqt(input: &[u8]) -> IResult<&[u8], Dqt> {
    let (input, marker) = tag(b"\xFF\xDB")(input)?;
    let (input, length) = be_u16(input)?;
    // Parse quantization table data based on length
    let (input, _) = take(length as usize - 2)(input)?;
    Ok((input, Dqt { identifier: *[0xFF, 0xDB].try_into().unwrap(), length }))
}

fn parse_sof(input: &[u8]) -> IResult<&[u8], Sof> {
    let (input, marker) = tag(b"\xFF\xC0")(input)?;
    let (input, length) = be_u16(input)?;
    // Parse other SOF fields based on length
    let (input, _) = take(length as usize - 2)(input)?;
    Ok((input, Sof { identifier: *[0xFF, 0xC0].try_into().unwrap(), length }))
}

fn parse_dht_segments(input: &[u8]) -> IResult<&[u8], Vec<Dht>> {
    let (input, segments) = nom::multi::many0(parse_dht)(input)?;
    Ok((input, segments))
}

fn parse_dht(input: &[u8]) -> IResult<&[u8], Dht> {
    let (input, marker) = tag(b"\xFF\xC4")(input)?;
    let (input, length) = be_u16(input)?;
    // Parse Huffman table data based on length
    let (input, _) = take(length as usize - 2)(input)?;
    Ok((input, Dht { identifier: *[0xFF, 0xC4].try_into().unwrap(), length }))
}

fn parse_sos(input: &[u8]) -> IResult<&[u8], Sos> {
    let (input, marker) = tag(b"\xFF\xDA")(input)?;
    let (input, length) = be_u16(input)?;
    // Parse other SOS fields based on length
    let (input, _) = take(length as usize - 2)(input)?;
    Ok((input, Sos { identifier: *[0xFF, 0xDA].try_into().unwrap(), length }))
}


fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <jpeg_file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let data = fs::read(filename)?;

    match parse_jpeg(&data) {
        Ok((remaining, header)) => {
            println!("JPEG Header: {:?}", header);
            if !remaining.is_empty() {
                println!("Remaining data: {:?}", remaining);
            }
        }
        Err(e) => {
            eprintln!("Error parsing JPEG: {}", e);
        }
    }

    Ok(())
}
