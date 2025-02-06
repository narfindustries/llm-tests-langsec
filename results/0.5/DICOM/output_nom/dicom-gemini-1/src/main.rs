use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct DicomElement {
    group: u16,
    element: u16,
    vr: String,
    length: u32,
    value: Vec<u8>,
}

fn dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, (group, element, vr_bytes, length)) = tuple((
        be_u16,
        be_u16,
        take(2usize),
        be_u32,
    ))(input)?;

    let vr = String::from_utf8_lossy(vr_bytes).into_owned();
    let (input, value) = take(length)(input)?;

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


fn dicom_dataset(input: &[u8]) -> IResult<&[u8], Vec<DicomElement>> {
    let mut elements = Vec::new();
    let mut remaining_input = input;

    loop {
        match dicom_element(remaining_input) {
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
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    match dicom_dataset(&buffer) {
        Ok((_, elements)) => {
            for element in elements {
                println!("{:?}", element);
            }
        }
        Err(err) => {
            println!("Error parsing DICOM file: {:?}", err);
        }
    }
}
