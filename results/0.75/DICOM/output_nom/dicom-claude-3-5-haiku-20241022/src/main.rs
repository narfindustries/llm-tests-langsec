use nom::{
    branch::alt,
    bytes::complete::{take, take_while},
    combinator::{map, opt},
    error::ParseError,
    multi::{count, length_count, length_data},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::{tuple, preceded},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct DicomHeader {
    preamble: [u8; 128],
    magic_number: [u8; 4],
}

#[derive(Debug)]
struct DicomTag {
    group: u16,
    element: u16,
}

#[derive(Debug)]
struct DicomElement {
    tag: DicomTag,
    vr: Option<String>,
    length: u32,
    value: Vec<u8>,
}

#[derive(Debug)]
struct DicomFile {
    header: DicomHeader,
    elements: Vec<DicomElement>,
}

fn parse_dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = take(128usize)(input)?;
    let (input, magic_number) = take(4usize)(input)?;

    Ok((input, DicomHeader {
        preamble: preamble.try_into().unwrap(),
        magic_number: magic_number.try_into().unwrap(),
    }))
}

fn parse_dicom_tag(input: &[u8]) -> IResult<&[u8], DicomTag> {
    let (input, group) = le_u16(input)?;
    let (input, element) = le_u16(input)?;

    Ok((input, DicomTag { group, element }))
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, tag) = parse_dicom_tag(input)?;
    let (input, vr) = opt(map(take(2usize), |vr: &[u8]| String::from_utf8_lossy(vr).to_string()))(input)?;
    let (input, length) = le_u32(input)?;
    let (input, value) = take(length as usize)(input)?;

    Ok((input, DicomElement {
        tag,
        vr,
        length,
        value: value.to_vec(),
    }))
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], DicomFile> {
    let (input, header) = parse_dicom_header(input)?;
    let (input, elements) = many0(parse_dicom_element)(input)?;

    Ok((input, DicomFile {
        header,
        elements,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dicom_file(&buffer) {
        Ok((_, dicom_file)) => {
            println!("Parsed DICOM file: {:?}", dicom_file);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse DICOM file: {:?}", e);
            Err(e.into())
        }
    }
}