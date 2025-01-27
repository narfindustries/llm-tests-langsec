extern crate nom;

use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;
use std::str;

#[derive(Debug)]
struct DicomFile {
    preamble: [u8; 128],
    prefix: String,
    elements: Vec<DicomElement>,
}

#[derive(Debug)]
struct DicomElement {
    tag: u32,
    vr: String,
    length: u32,
    value: Vec<u8>,
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, (group, element, vr, length)) = tuple((be_u16, be_u16, take(2usize), be_u32))(input)?;
    let tag = ((group as u32) << 16) | (element as u32);
    let vr = str::from_utf8(vr).unwrap().to_string();
    let (input, value) = take(length as usize)(input)?;
    Ok((input, DicomElement { tag, vr, length, value: value.to_vec() }))
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], DicomFile> {
    let (input, preamble) = take(128usize)(input)?;
    let preamble = <[u8; 128]>::try_from(preamble).unwrap();
    let (input, prefix) = map_res(take(4usize), str::from_utf8)(input)?;
    let mut elements = Vec::new();
    let mut current_input = input;
    while !current_input.is_empty() {
        match parse_dicom_element(current_input) {
            Ok((next_input, element)) => {
                elements.push(element);
                current_input = next_input;
            }
            Err(_) => break,
        }
    }
    Ok((current_input, DicomFile {
        preamble,
        prefix: prefix.to_string(),
        elements,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Could not open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");

    match parse_dicom_file(&buffer) {
        Ok((_, dicom_file)) => {
            println!("{:#?}", dicom_file);
        }
        Err(e) => {
            eprintln!("Failed to parse DICOM file: {:?}", e);
        }
    }
}
