use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{le_u16, le_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct DicomHeader {
    preamble: [u8; 128],
    magic_number: [u8; 4],
}

#[derive(Debug)]
struct DicomElement {
    tag: (u16, u16),
    vr: Option<String>,
    length: u32,
    value: Vec<u8>,
}

fn parse_preamble(input: &[u8]) -> IResult<&[u8], [u8; 128]> {
    let (input, preamble) = take(128usize)(input)?;
    Ok((input, preamble.try_into().unwrap()))
}

fn parse_magic_number(input: &[u8]) -> IResult<&[u8], [u8; 4]> {
    let (input, magic) = take(4usize)(input)?;
    Ok((input, magic.try_into().unwrap()))
}

fn parse_dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = parse_preamble(input)?;
    let (input, magic_number) = parse_magic_number(input)?;
    Ok((input, DicomHeader { preamble, magic_number }))
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, tag) = tuple((le_u16, le_u16))(input)?;
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

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], (DicomHeader, Vec<DicomElement>)> {
    let (input, header) = parse_dicom_header(input)?;
    let (input, elements) = many0(parse_dicom_element)(input)?;
    Ok((input, (header, elements)))
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
        Ok((_, (header, elements))) => {
            println!("DICOM Header: {:?}", header);
            println!("DICOM Elements: {:?}", elements);
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}