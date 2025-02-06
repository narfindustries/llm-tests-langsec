use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::be_u32,
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct DicomElement {
    tag: u32,
    vr: String,
    vl: u32,
    value: Vec<u8>,
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (rest, (tag, vr, vl)) = tuple((be_u32, parse_vr, be_u32))(input)?;
    let (rest, value) = take(vl as usize)(rest)?;
    Ok((rest, DicomElement { tag, vr: vr.to_string(), vl, value: value.to_vec() }))
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take(2usize), |vr_bytes| {
        std::str::from_utf8(vr_bytes)
    })(input)
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], Vec<DicomElement>> {
    let mut elements = Vec::new();
    let mut remaining_input = input;

    while !remaining_input.is_empty() {
        match parse_dicom_element(remaining_input) {
            Ok((rest, element)) => {
                elements.push(element);
                remaining_input = rest;
            }
            Err(e) => {
                println!("Error parsing DICOM element: {:?}", e);
                break;
            }
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
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file {}: {}", filename, err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file {}: {}", filename, err);
            return;
        }
    };

    match parse_dicom_file(&buffer) {
        Ok((_, elements)) => {
            for element in elements {
                println!("{:?}", element);
            }
        }
        Err(e) => {
            println!("Error parsing DICOM file: {:?}", e);
        }
    }
}
