use nom::{
    bytes::complete::{tag, take},
    multi::many_till,
    number::complete::be_u16,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

// Marker definitions
const SOI: u16 = 0xFFD8;
const EOI: u16 = 0xFFD9;
const APP0: u16 = 0xFFE0;
const APP15: u16 = 0xFFEF;
const DQT: u16 = 0xFFDB;
const SOF0: u16 = 0xFFC0;
const SOF15: u16 = 0xFFCF;
const DHT: u16 = 0xFFC4;
const SOS: u16 = 0xFFDA;
const COM: u16 = 0xFFFE;

// JPEG Parser
fn parse_marker(input: &[u8]) -> IResult<&[u8], u16> {
    be_u16(input)
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let (input, length) = be_u16(input)?;
    take((length - 2) as usize)(input)
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _soi) = tag(&SOI.to_be_bytes())(input)?;

    let (input, _) = many_till(
        |input| {
            let (input, marker) = parse_marker(input)?;
            match marker {
                APP0..=APP15 | DQT | SOF0..=SOF15 | DHT | SOS | COM => {
                    let (input, _) = parse_segment(input)?;
                    Ok((input, ()))
                }
                EOI => Ok((input, ())),
                _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            }
        },
        tag(&EOI.to_be_bytes()),
    )(input)?;

    Ok((input, ()))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Cannot open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Cannot read file");

    match parse_jpeg(&buffer) {
        Ok((_, _)) => println!("Parsed JPEG successfully."),
        Err(e) => eprintln!("Error parsing JPEG: {:?}", e),
    }
}