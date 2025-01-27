use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    number::complete::{be_u16, be_u32, be_i32},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct DicomHeader {
    prefix: [u8; 128],
    dicom_prefix: [u8; 4],
    version: (u8, u8),
    max_length: u32,
}

#[derive(Debug)]
struct DicomElement {
    group: u16,
    element: u16,
    length: u32,
    value: Vec<u8>,
}


fn dicom_prefix(input: &[u8]) -> IResult<&[u8], [u8; 4]> {
    map(take(4usize), |slice: &[u8]| {
        let mut array = [0u8; 4];
        array.copy_from_slice(slice);
        array
    })(input)
}

fn dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, prefix) = take(128usize)(input)?;
    let (input, dicom_prefix) = dicom_prefix(input)?;
    let (input, version) = tuple((be_u8, be_u8))(input)?;
    let (input, max_length) = be_u32(input)?;

    Ok((
        input,
        DicomHeader {
            prefix,
            dicom_prefix,
            version,
            max_length,
        },
    ))
}

fn dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, group) = be_u16(input)?;
    let (input, element) = be_u16(input)?;
    let (input, length) = be_u32(input)?;
    let (input, value) = take(length as usize)(input)?;

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


fn parse_dicom(input: &[u8]) -> IResult<&[u8], (DicomHeader, Vec<DicomElement>)> {
    let (input, header) = dicom_header(input)?;
    let (input, elements) = many0(dicom_element)(input)?;
    Ok((input, (header, elements)))
}

use nom::multi::many0;
use nom::number::complete::be_u8;

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

    match parse_dicom(&buffer) {
        Ok((_, (header, elements))) => {
            println!("DICOM Header: {:?}", header);
            println!("DICOM Elements: {:?}", elements);
        }
        Err(e) => {
            println!("Error parsing DICOM file: {:?}", e);
        }
    }
}
