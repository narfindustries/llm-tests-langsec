use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{many0, many1},
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
    magic_number: [u8; 4],
}

#[derive(Debug)]
struct DicomElement {
    tag: u32,
    vr: [u8; 2],
    length: u32,
    value: Vec<u8>,
}

#[derive(Debug)]
struct DicomFile {
    header: DicomHeader,
    elements: Vec<DicomElement>,
}

fn parse_preamble(input: &[u8]) -> IResult<&[u8], [u8; 128]> {
    let (input, preamble) = take(128usize)(input)?;
    Ok((input, preamble.try_into().unwrap()))
}

fn parse_magic_number(input: &[u8]) -> IResult<&[u8], [u8; 4]> {
    let (input, magic) = tag(b"DICM")(input)?;
    Ok((input, magic.try_into().unwrap()))
}

fn parse_dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = parse_preamble(input)?;
    let (input, magic_number) = parse_magic_number(input)?;
    Ok((input, DicomHeader { preamble, magic_number }))
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], [u8; 2]> {
    let (input, vr) = take(2usize)(input)?;
    Ok((input, vr.try_into().unwrap()))
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, tag) = le_u32(input)?;
    let (input, vr) = parse_vr(input)?;
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
    let (input, elements) = many1(parse_dicom_element)(input)?;
    
    Ok((input, DicomFile {
        header,
        elements,
    }))
}

fn main() -> std::io::Result<()> {
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
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}