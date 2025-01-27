use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct DicomHeader {
    preamble: Vec<u8>,
    prefix: Vec<u8>,
    group_length: u32,
}

#[derive(Debug)]
struct DicomElement {
    tag: (u16, u16),
    vr: String,
    length: u32,
    value: Vec<u8>,
}

#[derive(Debug)]
struct DicomFile {
    header: DicomHeader,
    elements: Vec<DicomElement>,
}

fn parse_preamble(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    take(128usize)(input)
}

fn parse_prefix(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    tag(b"DICM")(input).map(|(rest, prefix)| (rest, prefix.to_vec()))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, (preamble, prefix, group_length)) =
        tuple((parse_preamble, parse_prefix, be_u32))(input)?;
    Ok((
        input,
        DicomHeader {
            preamble: preamble.to_vec(),
            prefix: prefix.to_vec(),
            group_length,
        },
    ))
}

fn parse_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, (group, element)) = tuple((be_u16, be_u16))(input)?;
    let (input, vr) = take(2usize)(input)?;
    let (input, length) = be_u32(input)?;
    let (input, value) = take(length as usize)(input)?;
    Ok((
        input,
        DicomElement {
            tag: (group, element),
            vr: String::from_utf8_lossy(vr).to_string(),
            length,
            value: value.to_vec(),
        },
    ))
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], DicomFile> {
    let (input, header) = parse_header(input)?;
    let mut elements = Vec::new();
    let mut remaining_input = input;
    while !remaining_input.is_empty() {
        let (input, element) = parse_element(remaining_input)?;
        elements.push(element);
        remaining_input = input;
    }
    Ok((remaining_input, DicomFile { header, elements }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        std::process::exit(1);
    }
    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    let (_, dicom_file) = parse_dicom_file(&buffer).expect("Failed to parse DICOM file");
    println!("{:#?}", dicom_file);
    Ok(())
}