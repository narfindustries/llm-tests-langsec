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
    meta_elements: Vec<DicomMetaElement>,
}

#[derive(Debug)]
struct DicomMetaElement {
    tag: (u16, u16),
    vr: String,
    length: u16,
    value: Vec<u8>,
}

#[derive(Debug)]
struct DicomDataSet {
    elements: Vec<DicomElement>,
}

#[derive(Debug)]
struct DicomElement {
    tag: (u16, u16),
    vr: String,
    length: u32,
    value: Vec<u8>,
}

fn parse_preamble(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    take(128usize)(input)
}

fn parse_prefix(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    tag(b"DICM")(input).map(|(rest, prefix)| (rest, prefix.to_vec()))
}

fn parse_meta_element(input: &[u8]) -> IResult<&[u8], DicomMetaElement> {
    let (input, (tag1, tag2, vr, length, value)) = tuple((
        be_u16,
        be_u16,
        take(2usize),
        be_u16,
        take(16usize),
    ))(input)?;

    Ok((
        input,
        DicomMetaElement {
            tag: (tag1, tag2),
            vr: String::from_utf8_lossy(vr).to_string(),
            length,
            value: value.to_vec(),
        },
    ))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = parse_preamble(input)?;
    let (input, prefix) = parse_prefix(input)?;
    let (input, meta_elements) = nom::multi::many0(parse_meta_element)(input)?;

    Ok((
        input,
        DicomHeader {
            preamble,
            prefix,
            meta_elements,
        },
    ))
}

fn parse_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, (tag1, tag2, vr, length, value)) = tuple((
        be_u16,
        be_u16,
        take(2usize),
        be_u32,
        take(16usize),
    ))(input)?;

    Ok((
        input,
        DicomElement {
            tag: (tag1, tag2),
            vr: String::from_utf8_lossy(vr).to_string(),
            length,
            value: value.to_vec(),
        },
    ))
}

fn parse_data_set(input: &[u8]) -> IResult<&[u8], DicomDataSet> {
    let (input, elements) = nom::multi::many0(parse_element)(input)?;

    Ok((input, DicomDataSet { elements }))
}

fn parse_dicom(input: &[u8]) -> IResult<&[u8], (DicomHeader, DicomDataSet)> {
    let (input, header) = parse_header(input)?;
    let (input, data_set) = parse_data_set(input)?;

    Ok((input, (header, data_set)))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dicom(&buffer) {
        Ok((_, (header, data_set))) => {
            println!("{:#?}", header);
            println!("{:#?}", data_set);
        }
        Err(e) => eprintln!("Failed to parse DICOM file: {:?}", e),
    }

    Ok(())
}