use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    error::ParseError,
    multi::many1,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct DICOMHeader {
    preamble: [u8; 128],
    magic: [u8; 4],
}

#[derive(Debug)]
struct DICOMElement {
    tag: (u16, u16),
    vr: Option<String>,
    length: u32,
    value: Vec<u8>,
}

#[derive(Debug)]
struct DICOMFile {
    header: DICOMHeader,
    elements: Vec<DICOMElement>,
}

fn parse_dicom_header<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], DICOMHeader, E> {
    map(
        tuple((
            take::<usize, &'a [u8], E>(128),
            tag(b"DICM")
        )),
        |(preamble, magic)| DICOMHeader {
            preamble: preamble.try_into().unwrap(),
            magic: magic.try_into().unwrap(),
        }
    )(input)
}

fn parse_dicom_element<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], DICOMElement, E> {
    let (input, group) = le_u16(input)?;
    let (input, element) = le_u16(input)?;
    let (input, vr) = opt(map(take::<usize, &'a [u8], E>(2), |vr: &[u8]| String::from_utf8_lossy(vr).to_string()))(input)?;
    let (input, length) = le_u32(input)?;
    let (input, value) = take::<u32, &'a [u8], E>(length)(input)?;

    Ok((input, DICOMElement {
        tag: (group, element),
        vr,
        length,
        value: value.to_vec(),
    }))
}

fn parse_dicom_file<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], DICOMFile, E> {
    map(
        tuple((
            parse_dicom_header,
            many1(parse_dicom_element)
        )),
        |(header, elements)| DICOMFile { header, elements }
    )(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dicom_file::<nom::error::Error<&[u8]>>(&buffer) {
        Ok((_, dicom_file)) => {
            println!("Parsed DICOM File: {:?}", dicom_file);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1)
        }
    }
}