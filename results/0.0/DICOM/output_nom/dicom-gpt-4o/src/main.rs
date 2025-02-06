use nom::{
    bytes::complete::take,
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct DicomElement {
    group: u16,
    element: u16,
    length: u32,
    value: Vec<u8>,
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, group) = le_u16(input)?;
    let (input, element) = le_u16(input)?;
    let (input, length) = le_u32(input)?;
    let (input, value) = take(length)(input)?;

    Ok((
        input,
        DicomElement {
            group,
            element,
            length,
            value: value.to_vec(),
        },
    ))
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], Vec<DicomElement>> {
    let mut elements = Vec::new();
    let mut remaining_input = input;

    while !remaining_input.is_empty() {
        match parse_dicom_element(remaining_input) {
            Ok((input, element)) => {
                elements.push(element);
                remaining_input = input;
            }
            Err(_) => break,
        }
    }

    Ok((remaining_input, elements))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dicom_file(&buffer) {
        Ok((_, elements)) => {
            for element in elements {
                println!("{:?}", element);
            }
        }
        Err(e) => eprintln!("Failed to parse DICOM file: {:?}", e),
    }

    Ok(())
}