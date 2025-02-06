use nom::{
    bytes::complete::take,
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

fn parse_tag(input: &[u8]) -> IResult<&[u8], (u16, u16)> {
    let (input, group) = le_u16(input)?;
    let (input, element) = le_u16(input)?;
    Ok((input, (group, element)))
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], String> {
    let (input, vr_bytes) = take(2usize)(input)?;
    let vr = String::from_utf8_lossy(vr_bytes).to_string();
    Ok((input, vr))
}

fn parse_length(input: &[u8]) -> IResult<&[u8], u32> {
    le_u32(input)
}

fn parse_value(input: &[u8], length: u32) -> IResult<&[u8], Vec<u8>> {
    let (input, value_bytes) = take(length as usize)(input)?;
    Ok((input, value_bytes.to_vec()))
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, tag) = parse_tag(input)?;
    let (input, vr) = parse_vr(input)?;
    let (input, length) = parse_length(input)?;
    let (input, value) = parse_value(input, length)?;

    Ok((
        input,
        DicomElement {
            tag,
            vr,
            length,
            value,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <DICOM file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let mut input = &buffer[..];
    while !input.is_empty() {
        match parse_dicom_element(input) {
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

    Ok(())
}