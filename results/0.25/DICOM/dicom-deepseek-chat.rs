use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

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
    let (input, (preamble, prefix, group, element, vr, length, value)) = tuple((
        parse_preamble,
        parse_prefix,
        be_u16,
        be_u16,
        take(2usize),
        be_u32,
        take(4usize),
    ))(input)?;

    Ok((
        input,
        DicomHeader {
            preamble: preamble.to_vec(),
            prefix: prefix.to_vec(),
            group,
            element,
            vr: vr.to_vec(),
            length,
            value: value.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Failed to parse DICOM file: {:?}", e),
    }
}