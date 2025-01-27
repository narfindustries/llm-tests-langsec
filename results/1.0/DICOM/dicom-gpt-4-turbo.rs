use nom::{
    bytes::complete::{take, take_until},
    combinator::{map_res, opt},
    number::complete::{le_u16, le_u32},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

const DICOM_PREFIX: &[u8; 128] = &[0; 128];
const DICOM_MAGIC: &str = "DICM";

#[derive(Debug)]
struct DicomHeader {
    group_number: u16,
    element_number: u16,
    vr: String,
    value_length: u32,
    value: Vec<u8>,
}

#[derive(Debug)]
struct DicomFile {
    prefix: Vec<u8>,
    magic: String,
    headers: Vec<DicomHeader>,
}

fn parse_dicom<'a>(input: &'a [u8]) -> IResult<&'a [u8], DicomFile> {
    let (input, prefix) = take(128u32)(input)?;
    let (input, magic) = map_res(take_until("\0"), |s: &[u8]| std::str::from_utf8(s))(input)?;
    let (input, _) = take(4u32)(input)?; // skip null terminators after "DICM"
    let (mut input, mut headers) = (input, vec![]);

    while !input.is_empty() {
        match parse_header(input) {
            Ok((i, header)) => {
                headers.push(header);
                input = i;
            }
            Err(_) => break,
        }
    }

    Ok((input, DicomFile {
        prefix: prefix.to_vec(),
        magic: magic.to_string(),
        headers,
    }))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, (group_number, element_number)) = tuple((le_u16, le_u16))(input)?;
    let (input, vr) = map_res(take(2u16), |s: &[u8]| std::str::from_utf8(s))(input)?;
    let (input, value_length) = value_length_by_vr(vr, input)?;
    let (input, value) = take(value_length)(input)?;

    Ok((
        input,
        DicomHeader {
            group_number,
            element_number,
            vr: vr.to_string(),
            value_length,
            value: value.to_vec(),
        },
    ))
}

fn value_length_by_vr(vr: &str, input: &[u8]) -> IResult<&[u8], u32> {
    match vr {
        "OB" | "OW" | "OF" | "SQ" | "UT" | "UN" => {
            let (input, _) = take(2u16)(input)?;
            let (input, length) = le_u32(input)?;
            Ok((input, length))
        }
        _ => {
            let (input, length) = le_u16(input)?;
            Ok((input, length as u32))
        }
    }
}

fn main() -> io::Result<()> {
    let path = std::env::args().nth(1).expect("Please provide a file path");
    let mut file = File::open(Path::new(&path))?;
    let mut buffer = Vec::new();
    
    file.read_to_end(&mut buffer)?;
    
    match parse_dicom(&buffer) {
        Ok((_, dicom_file)) => println!("{:?}", dicom_file),
        Err(e) => println!("Failed to parse DICOM file: {:?}", e),
    }

    Ok(())
}