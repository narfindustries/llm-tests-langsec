use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug, PartialEq)]
pub struct DicomHeader {
    pub group_number: u16,
    pub element_number: u16,
    pub vr: Option<String>,
    pub length: u32,
    pub value: Vec<u8>,
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], String> {
    let (input, vr) = take(2usize)(input)?;
    Ok((input, String::from_utf8_lossy(vr).to_string()))
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, group_number) = le_u16(input)?;
    let (input, element_number) = le_u16(input)?;
    let (input, vr) = parse_vr(input)?;
    let vr_code = vr.as_str();

    let (input, length) = if ["OB", "OW", "OF", "SQ", "UT", "UN"].contains(&vr_code) {
        let (input, _) = take(2usize)(input)?; // reserved bytes
        let (input, length) = le_u32(input)?;
        (input, length)
    } else {
        let (input, length) = le_u16(input)?;
        (input, length as u32)
    };

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
    let mut input = input;
    let mut headers = Vec::new();

    while !input.is_empty() {
        let result = parse_dicom_element(input);
        match result {
            Ok((i, header)) => {
                headers.push(header);
                input = i;
            }
            Err(_) => break,
        }
    }

    Ok((input, headers))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} <DICOM file path>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(error) => {
            println!("Error opening file: {}", error);
            return;
        },
    };

    let mut data = Vec::new();
    if let Err(error) = file.read_to_end(&mut data) {
        println!("Error reading file: {}", error);
        return;
    }

    match parse_dicom_file(&data) {
        Ok((_, headers)) => {
            for header in headers {
                println!("{:?}", header);
            }
        }
        Err(error) => println!("Error parsing DICOM data: {:?}", error),
    }
}