use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct DicomElement {
    tag: (u16, u16),
    vr: String,
    length: u32,
    value: Vec<u8>,
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, (group, element)) = tuple((be_u16, be_u16))(input)?;
    let (input, vr) = take(2usize)(input)?;
    let (input, length) = be_u32(input)?;
    let (input, value) = take(length)(input)?;

    Ok((
        input,
        DicomElement {
            tag: (group, element),
            vr: String::from_utf8_lossy(vr).to_string(),
            length,
            value: value.to_vec(),
        },
    ))
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], Vec<DicomElement>> {
    let mut elements = Vec::new();
    let mut remaining = input;

    while !remaining.is_empty() {
        let (new_remaining, element) = parse_dicom_element(remaining)?;
        elements.push(element);
        remaining = new_remaining;
    }

    Ok((remaining, elements))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_dicom_file(&buffer) {
        Ok((_, elements)) => {
            for element in elements {
                println!("{:?}", element);
            }
        }
        Err(e) => eprintln!("Failed to parse DICOM file: {:?}", e),
    }
}