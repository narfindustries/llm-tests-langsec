use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::many1,
    branch::alt,
    IResult,
    number::complete::{le_u16, le_u32},
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::error::Error;

#[derive(Debug)]
struct DicomHeader {
    preamble: [u8; 128],
    magic_number: [u8; 4],
}

#[derive(Debug)]
struct DicomElement {
    group: u16,
    element: u16,
    vr: Option<String>,
    length: u32,
    value: Vec<u8>,
}

#[derive(Debug)]
struct DicomFile {
    header: DicomHeader,
    elements: Vec<DicomElement>,
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], Option<String>> {
    alt((
        map(tag(b"AE"), |_| Some("Application Entity".to_string())),
        map(tag(b"AS"), |_| Some("Age String".to_string())),
        map(tag(b"AT"), |_| Some("Attribute Tag".to_string())),
        map(tag(b"CS"), |_| Some("Code String".to_string())),
        map(tag(b"DA"), |_| Some("Date".to_string())),
        map(tag(b"DS"), |_| Some("Decimal String".to_string())),
        map(tag(b"DT"), |_| Some("Date Time".to_string())),
        map(tag(b"FL"), |_| Some("Floating Point Single".to_string())),
        map(tag(b"FD"), |_| Some("Floating Point Double".to_string())),
        map(tag(b"IS"), |_| Some("Integer String".to_string())),
        map(tag(b"LO"), |_| Some("Long String".to_string())),
        map(tag(b"LT"), |_| Some("Long Text".to_string())),
        map(tag(b"OB"), |_| Some("Other Byte".to_string())),
        map(tag(b"OD"), |_| Some("Other Double".to_string())),
        map(tag(b"OF"), |_| Some("Other Float".to_string())),
        map(tag(b"OL"), |_| Some("Other Long".to_string())),
        map(tag(b"OW"), |_| Some("Other Word".to_string())),
        map(tag(b"PN"), |_| Some("Person Name".to_string())),
        map(tag(b"SH"), |_| Some("Short String".to_string())),
        map(tag(b"SL"), |_| Some("Signed Long".to_string())),
        map(tag(b"SQ"), |_| Some("Sequence of Items".to_string())),
        map(tag(b"SS"), |_| Some("Signed Short".to_string())),
        map(tag(b"ST"), |_| Some("Short Text".to_string())),
        map(tag(b"TM"), |_| Some("Time".to_string())),
        map(tag(b"UI"), |_| Some("Unique Identifier".to_string())),
        map(tag(b"UL"), |_| Some("Unsigned Long".to_string())),
        map(tag(b"UN"), |_| Some("Unknown".to_string())),
        map(tag(b"UR"), |_| Some("URI/URL".to_string())),
        map(tag(b"US"), |_| Some("Unsigned Short".to_string())),
        map(tag(b"UT"), |_| Some("Unlimited Text".to_string())),
        map(take(2usize), |_| None)
    ))
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, group) = le_u16(input)?;
    let (input, element) = le_u16(input)?;
    let (input, vr) = opt(parse_vr)(input)?;
    let (input, length) = le_u32(input)?;
    let (input, value) = take(length as usize)(input)?;

    Ok((input, DicomElement {
        group,
        element,
        vr: vr.flatten(),
        length,
        value: value.to_vec(),
    }))
}

fn parse_dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = take(128usize)(input)?;
    let (input, magic_number) = tag(b"DICM")(input)?;

    Ok((input, DicomHeader {
        preamble: preamble.try_into().unwrap(),
        magic_number: magic_number.try_into().unwrap(),
    }))
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], DicomFile> {
    let (input, header) = parse_dicom_header(input)?;
    let (input, elements) = many1(parse_dicom_element)(input)?;

    Ok((input, DicomFile {
        header,
        elements,
    }))
}

fn main() -> Result<(), Box<dyn Error>> {
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
            eprintln!("Error parsing DICOM file: {:?}", e);
            Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidData, 
                format!("Parsing error: {:?}", e)
            )))
        }
    }
}