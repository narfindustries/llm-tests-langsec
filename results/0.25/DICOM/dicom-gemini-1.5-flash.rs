use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct DicomHeader {
    preamble: [u8; 128],
    dicom_prefix: [u8; 4],
    group_length: u32,
}

fn parse_dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = take(128usize)(input)?;
    let (input, dicom_prefix) = tag(b"DICM")(input)?;
    let (input, group_length) = be_u32(input)?;

    Ok((
        input,
        DicomHeader {
            preamble: preamble.try_into().unwrap(),
            dicom_prefix: dicom_prefix.try_into().unwrap(),
            group_length,
        },
    ))
}


#[derive(Debug)]
struct DicomElement {
    group: u16,
    element: u16,
    vr: String,
    length: u32,
    value: Vec<u8>,
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, (group, element, vr_bytes, length)) = tuple((
        be_u16,
        be_u16,
        take(2usize),
        be_u32,
    ))(input)?;

    let vr = String::from_utf8_lossy(vr_bytes).to_string();
    let (input, value) = take(length as usize)(input)?;

    Ok((
        input,
        DicomElement {
            group,
            element,
            vr,
            length,
            value: value.to_vec(),
        },
    ))
}

fn parse_dicom_data(input: &[u8]) -> IResult<&[u8], Vec<DicomElement>> {
    let mut elements = Vec::new();
    let mut remaining_input = input;
    loop {
        match parse_dicom_element(remaining_input) {
            Ok((rest, element)) => {
                elements.push(element);
                remaining_input = rest;
            }
            Err(_) => break,
        }
    }
    Ok((remaining_input, elements))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <dicom_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_dicom_header(&buffer) {
        Ok((rest, header)) => {
            println!("DICOM Header: {:?}", header);
            match parse_dicom_data(rest) {
                Ok((_, elements)) => {
                    println!("DICOM Elements: {:?}", elements);
                }
                Err(e) => println!("Error parsing DICOM data: {:?}", e),
            }
        }
        Err(e) => println!("Error parsing DICOM header: {:?}", e),
    }
}
