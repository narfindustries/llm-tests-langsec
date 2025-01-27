use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opts},
    error::{context, ErrorKind, ParseError},
    multi::take_till,
    number::complete::{be_u16, be_u8},
    sequence::{pair, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, stdin};
use std::path::Path;

// Enum for JPEG markers
#[derive(Debug, PartialEq)]
enum Marker {
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
    DQT,
    SOF0,
    SOF1,
    SOF2,
    SOF3,
    DHT,
    SOI2,
    DRI,
    SOS,
    EOI,
    TEM,
    RST0,
    RST1,
    RST2,
    RST3,
    RST4,
    RST5,
    RST6,
    RST7,
}

// Function to parse Marker
fn parse_marker(input: &[u8]) -> IResult<&[u8], Marker> {
    context(
        "Marker",
        map_res(be_u8, |x| match x {
            0xD8 => Ok(Marker::SOI),
            0xE0 => Ok(Marker::APP0),
            0xE1 => Ok(Marker::APP1),
            0xE2 => Ok(Marker::APP2),
            0xE3 => Ok(Marker::APP3),
            0xE4 => Ok(Marker::APP4),
            0xE5 => Ok(Marker::APP5),
            0xE6 => Ok(Marker::APP6),
            0xE7 => Ok(Marker::APP7),
            0xE8 => Ok(Marker::APP8),
            0xE9 => Ok(Marker::APP9),
            0xEA => Ok(Marker::APP10),
            0xEB => Ok(Marker::APP11),
            0xEC => Ok(Marker::APP12),
            0xED => Ok(Marker::APP13),
            0xEE => Ok(Marker::APP14),
            0xEF => Ok(Marker::APP15),
            0xDB => Ok(Marker::DQT),
            0xC0 => Ok(Marker::SOF0),
            0xC1 => Ok(Marker::SOF1),
            0xC2 => Ok(Marker::SOF2),
            0xC3 => Ok(Marker::SOF3),
            0xC4 => Ok(Marker::DHT),
            0xD9 => Ok(Marker::EOI),
            0xDA => Ok(Marker::SOS),
            _ => Err("Invalid marker"),
        }),
    )(input)
}

// Function to parse APP0
fn parse_app0(input: &[u8]) -> IResult<&[u8], &[u8]> {
    context(
        "APP0",
        map(pair(be_u16, take_till(|x| x == 0xFF)), |(size, data)| data),
    )(input)
}

// Function to parse DQT
fn parse_dqt(input: &[u8]) -> IResult<&[u8], &[u8]> {
    context(
        "DQT",
        map(pair(be_u8, take_till(|x| x == 0xFF)), |(precision, data)| data),
    )(input)
}

// Function to parse SOF0
fn parse_sof0(input: &[u8]) -> IResult<&[u8], &[u8]> {
    context(
        "SOF0",
        map(
            tuple((be_u8, be_u16, be_u16, be_u8, be_u8)),
            |(bits, height, width, channels, _)| {
                &input[5..]
            },
        ),
    )(input)
}

// Function to parse SOS
fn parse_sos(input: &[u8]) -> IResult<&[u8], &[u8]> {
    context(
        "SOS",
        map(
            tuple((be_u8, be_u8, be_u8, be_u8, be_u8, take_till(|x| x == 0xFF))),
            |(components, start, end, ah, al, _)| &input[6..],
        ),
    )(input)
}

// Function to parse JPEG
fn parse_jpeg(input: &[u8]) -> IResult<&[u8], &[u8]> {
    context(
        "JPEG",
        map(
            pair(
                parse_marker,
                map(
                    opts!(parse_app0, parse_dqt, parse_sof0, parse_sos),
                    |app0| {
                        &input[2 + app0.len()..]
                    },
                ),
            ),
            |(_, _)| &input[2..],
        ),
    )(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        let mut file = File::open(&args[1])?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;
        match parse_jpeg(&buffer) {
            Ok((input, _)) => {
                println!("Remaining input: {:?}", input);
            }
            Err(e) => {
                println!("Error: {:?}", e);
            }
        }
    } else {
        let mut buffer = Vec::new();
        stdin().read_to_end(&mut buffer)?;
        match parse_jpeg(&buffer) {
            Ok((input, _)) => {
                println!("Remaining input: {:?}", input);
            }
            Err(e) => {
                println!("Error: {:?}", e);
            }
        }
    }
    Ok(())
}