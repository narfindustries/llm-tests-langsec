use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    fs::File,
    io::Read,
    env,
};

#[derive(Debug)]
struct DicomHeader {
    preamble: Vec<u8>,
    prefix: Vec<u8>,
    group: u16,
    element: u16,
    vr: Vec<u8>,
    length: u32,
    value: Vec<u8>,
}

fn parse_preamble(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    take(128usize)(input)
}

fn parse_prefix(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    tag(b"DICM")(input).map(|(rest, prefix)| (rest, prefix.to_vec()))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, (group, element, vr, length, value)) = tuple((
        be_u16,
        be_u16,
        take(2usize),
        be_u32,
        |input: &[u8]| take(length)(input),
    ))(input)?;

    Ok((
        input,
        DicomHeader {
            preamble: Vec::new(),
            prefix: Vec::new(),
            group,
            element,
            vr: vr.to_vec(),
            length,
            value: value.to_vec(),
        },
    ))
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], Vec<DicomHeader>> {
    let (input, preamble) = parse_preamble(input)?;
    let (input, prefix) = parse_prefix(input)?;
    let mut headers = Vec::new();
    let mut remaining_input = input;

    while !remaining_input.is_empty() {
        let (input, header) = parse_header(remaining_input)?;
        headers.push(DicomHeader {
            preamble: preamble.clone(),
            prefix: prefix.clone(),
            ..header
        });
        remaining_input = input;
    }

    Ok((remaining_input, headers))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_dicom_file(&buffer) {
        Ok((_, headers)) => {
            for header in headers {
                println!("{:?}", header);
            }
        }
        Err(e) => eprintln!("Failed to parse DICOM file: {:?}", e),
    }
}