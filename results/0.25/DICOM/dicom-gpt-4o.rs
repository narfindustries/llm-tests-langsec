extern crate nom;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::many0,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;
use std::str;

#[derive(Debug)]
struct DicomElement {
    tag: u32,
    vr: String,
    length: u32,
    value: Vec<u8>,
}

fn parse_tag(input: &[u8]) -> IResult<&[u8], u32> {
    map(tuple((be_u16, be_u16)), |(group, element)| {
        ((group as u32) << 16) | (element as u32)
    })(input)
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take(2usize), |bytes: &[u8]| {
        str::from_utf8(bytes).map(|s| s.to_string())
    })(input)
}

fn parse_length(input: &[u8], vr: &str) -> IResult<&[u8], u32> {
    match vr {
        "OB" | "OW" | "OF" | "SQ" | "UT" | "UN" => {
            let (input, _) = tag([0u8, 0u8])(input)?;
            be_u32(input)
        }
        _ => map(be_u16, |len| len as u32)(input),
    }
}

fn parse_value(input: &[u8], length: u32) -> IResult<&[u8], Vec<u8>> {
    map(take(length), |bytes: &[u8]| bytes.to_vec())(input)
}

fn parse_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, tag) = parse_tag(input)?;
    let (input, vr) = parse_vr(input)?;
    let (input, length) = parse_length(input, &vr)?;
    let (input, value) = parse_value(input, length)?;

    Ok((input, DicomElement { tag, vr, length, value }))
}

fn parse_dicom(input: &[u8]) -> IResult<&[u8], Vec<DicomElement>> {
    many0(parse_element)(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Could not open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");

    match parse_dicom(&buffer) {
        Ok((_, elements)) => {
            for element in elements {
                println!("{:?}", element);
            }
        }
        Err(e) => eprintln!("Failed to parse DICOM file: {:?}", e),
    }
}