use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
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
    vr: [u8; 2],
    length: u32,
    value: Vec<u8>,
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, (group, element)) = tuple((le_u16, le_u16))(input)?;
    let (input, vr) = take(2usize)(input)?;
    let (input, length) = le_u32(input)?;
    let (input, value) = take(length as usize)(input)?;
    Ok((
        input,
        DicomElement {
            group,
            element,
            vr: *<&[u8; 2]>::try_from(vr).unwrap(),
            length,
            value: value.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <DICOM file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let mut input = &buffer[..];
    while !input.is_empty() {
        match parse_dicom_element(input) {
            Ok((remaining, element)) => {
                println!("{:?}", element);
                input = remaining;
            }
            Err(_) => break,
        }
    }
}