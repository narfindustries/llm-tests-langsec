use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct DicomElement {
    tag: (u16, u16),
    vr: String,
    length: u32,
    value: Vec<u8>,
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, tag_group) = le_u16(input)?;
    let (input, tag_element) = le_u16(input)?;
    let (input, vr) = take(2usize)(input)?;
    let vr = String::from_utf8(vr.to_vec()).unwrap();
    let (input, length) = if ["OB", "OW", "OF", "SQ", "UT", "UN"].contains(&vr.as_str()) {
        let (input, _) = take(2usize)(input)?; // Reserved bytes
        let (input, len) = le_u32(input)?;
        (input, len)
    } else {
        let (input, len) = le_u16(input)?;
        (input, len as u32)
    };
    let (input, value) = take(length)(input)?;

    Ok((
        input,
        DicomElement {
            tag: (tag_group, tag_element),
            vr,
            length,
            value: value.to_vec(),
        },
    ))
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], Vec<DicomElement>> {
    let mut elements = Vec::new();
    let mut remaining = input;
    while !remaining.is_empty() {
        let result = parse_dicom_element(remaining);
        match result {
            Ok((input, element)) => {
                elements.push(element);
                remaining = input;
            }
            Err(_) => break,
        }
    }
    Ok((remaining, elements))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <DICOM file path>", args[0]);
        return Ok(());
    }

    let file_path = &args[1];
    let mut file = File::open(file_path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dicom_file(&buffer) {
        Ok((_, elements)) => {
            for element in elements {
                println!("{:?}", element);
            }
        }
        Err(e) => println!("Error parsing DICOM file: {:?}", e),
    }

    Ok(())
}