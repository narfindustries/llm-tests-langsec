use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct DicomFile {
    preamble: Vec<u8>,
    prefix: Vec<u8>,
    elements: Vec<DataElement>,
}

#[derive(Debug)]
struct DataElement {
    tag_group: u16,
    tag_element: u16,
    vr: String,
    length: u32,
    value: Vec<u8>,
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], DicomFile> {
    let (input, preamble) = take(128usize)(input)?;
    let (input, prefix) = tag(b"DICM")(input)?;

    let mut elements = Vec::new();
    let mut remaining_input = input;
    while !remaining_input.is_empty() {
        let result = parse_data_element(remaining_input);
        match result {
            Ok((rest, element)) => {
                elements.push(element);
                remaining_input = rest;
            }
            Err(_) => break,
        }
    }

    Ok((
        remaining_input,
        DicomFile {
            preamble: preamble.to_vec(),
            prefix: prefix.to_vec(),
            elements,
        },
    ))
}

fn parse_data_element(input: &[u8]) -> IResult<&[u8], DataElement> {
    let (input, tag_group) = le_u16(input)?;
    let (input, tag_element) = le_u16(input)?;
    let (input, vr) = take(2usize)(input)?;
    let (input, length) = if vr == b"OB" || vr == b"OW" || vr == b"SQ" || vr == b"UN" {
        let (input, _) = take(2usize)(input)?;
        le_u32(input)
    } else {
        let (input, length) = le_u16(input)?;
        (input, length as u32)
    };
    let (input, value) = take(length)(input)?;

    Ok((
        input,
        DataElement {
            tag_group,
            tag_element,
            vr: String::from_utf8(vr.to_vec()).unwrap(),
            length,
            value: value.to_vec(),
        },
    ))
}

fn read_file(path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;
    Ok(contents)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <DICOM file path>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    match read_file(file_path) {
        Ok(contents) => match parse_dicom_file(&contents) {
            Ok((_, dicom_file)) => {
                println!("Parsed DICOM File: {:#?}", dicom_file);
            }
            Err(e) => {
                eprintln!("Error parsing DICOM file: {:?}", e);
            }
        },
        Err(e) => {
            eprintln!("Failed to read file {}: {:?}", file_path, e);
        }
    }
}