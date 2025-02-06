use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct DicomHeader {
    group: u16,
    element: u16,
    length: u32,
}

#[derive(Debug)]
struct DicomElement {
    header: DicomHeader,
    value: Vec<u8>,
}

fn parse_u16(input: &[u8]) -> IResult<&[u8], u16> {
    be_u16(input)
}

fn parse_u32(input: &[u8]) -> IResult<&[u8], u32> {
    be_u32(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, group) = parse_u16(input)?;
    let (input, element) = parse_u16(input)?;
    let (input, length) = parse_u32(input)?;
    Ok((input, DicomHeader { group, element, length }))
}

fn parse_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, header) = parse_header(input)?;
    let (input, value) = take(header.length)(input)?;
    Ok((input, DicomElement { header, value: value.to_vec() }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let mut input = &buffer[..];
    while !input.is_empty() {
        match parse_element(input) {
            Ok((remaining, element)) => {
                println!("{:?}", element);
                input = remaining;
            }
            Err(_) => {
                eprintln!("Failed to parse DICOM element");
                break;
            }
        }
    }
}