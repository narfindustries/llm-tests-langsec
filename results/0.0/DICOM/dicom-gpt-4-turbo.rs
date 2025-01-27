use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct DicomHeader {
    group_number: u16,
    element_number: u16,
    vr: Option<String>,
    length: u32,
    value: Vec<u8>,
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], String> {
    let (input, vr) = take(2usize)(input)?;
    Ok((input, String::from_utf8_lossy(vr).to_string()))
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, group_number) = le_u16(input)?;
    let (input, element_number) = le_u16(input)?;
    let (input, vr) = parse_vr(input)?;
    let (input, length) = le_u32(input)?;
    let (input, value) = take(length)(input)?;

    Ok((
        input,
        DicomHeader {
            group_number,
            element_number,
            vr: Some(vr),
            length,
            value: value.to_vec(),
        },
    ))
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], Vec<DicomHeader>> {
    let mut result = Vec::new();
    let mut remaining = input;

    while !remaining.is_empty() {
        let (next_input, element) = parse_dicom_element(remaining)?;
        result.push(element);
        remaining = next_input;
    }

    Ok((remaining, result))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <DICOM_FILE>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
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