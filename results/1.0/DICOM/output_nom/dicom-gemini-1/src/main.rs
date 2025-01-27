use nom::{
    bytes::complete::{tag, take, take_while, take_while1},
    combinator::{map, map_res, opt, recognize},
    number::complete::{be_u16, be_u32, be_i16},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct DicomElement {
    group: u16,
    element: u16,
    length: u32,
    value: Vec<u8>,
}

fn dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, group) = be_u16(input)?;
    let (input, element) = be_u16(input)?;
    let (input, length) = be_u32(input)?;
    let (input, value) = take(length)(input)?;
    Ok((input, DicomElement { group, element, length: length, value: value.to_vec() }))
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
        println!("Usage: dicom_parser <dicom_file>");
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match dicom_dataset(&buffer) {
        Ok((_, elements)) => {
            for element in elements {
                println!("Group: {}, Element: {}, Length: {}, Value: {:?}", element.group, element.element, element.length, element.value);
            }
        }
        Err(e) => {
            println!("Error parsing DICOM file: {:?}", e);
        }
    }
}
