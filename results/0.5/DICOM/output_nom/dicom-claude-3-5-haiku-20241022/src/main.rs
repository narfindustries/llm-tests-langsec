use nom::{
    bytes::complete::{take, take_until},
    combinator::{map, opt},
    multi::{count, many0, many_m_n},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct DicomHeader {
    preamble: [u8; 128],
    magic: [u8; 4],
}

#[derive(Debug)]
struct DicomElement {
    tag: (u16, u16),
    vr: Option<String>,
    length: u32,
    value: Vec<u8>,
}

fn parse_dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    map(
        tuple((
            take(128usize),
            take(4usize)
        )),
        |(preamble, magic)| DicomHeader {
            preamble: preamble.try_into().unwrap(),
            magic: magic.try_into().unwrap(),
        }
    )(input)
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    map(
        tuple((
            tuple((le_u16, le_u16)),
            opt(map(take(2usize), |vr: &[u8]| String::from_utf8_lossy(vr).to_string())),
            le_u32,
            take_until("\0")
        )),
        |(tag, vr, length, value)| DicomElement {
            tag,
            vr,
            length,
            value: value.to_vec(),
        }
    )(input)
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], (DicomHeader, Vec<DicomElement>)> {
    tuple((
        parse_dicom_header,
        many0(parse_dicom_element)
    ))(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dicom_file(&buffer) {
        Ok((_, (header, elements))) => {
            println!("DICOM Header: {:?}", header);
            println!("DICOM Elements: {:?}", elements);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            Err(e.into())
        }
    }
}