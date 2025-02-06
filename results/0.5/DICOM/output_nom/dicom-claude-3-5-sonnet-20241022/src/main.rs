use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct DicomHeader {
    preamble: Vec<u8>,
    prefix: Vec<u8>,
    meta_group: u16,
    meta_element: u16,
    meta_length: u32,
    transfer_syntax: String,
}

#[derive(Debug)]
struct DicomElement {
    group: u16,
    element: u16,
    vr: Option<String>,
    length: u32,
    value: Vec<u8>,
}

#[derive(Debug)]
struct DicomFile {
    header: DicomHeader,
    elements: Vec<DicomElement>,
}

fn parse_preamble(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, preamble) = take(128usize)(input)?;
    Ok((input, preamble.to_vec()))
}

fn parse_prefix(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, prefix) = tag("DICM")(input)?;
    Ok((input, prefix.to_vec()))
}

fn parse_meta_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = parse_preamble(input)?;
    let (input, prefix) = parse_prefix(input)?;
    let (input, (meta_group, meta_element)) = tuple((be_u16, be_u16))(input)?;
    let (input, meta_length) = be_u32(input)?;
    let (input, transfer_syntax_bytes) = take(meta_length)(input)?;
    
    Ok((input, DicomHeader {
        preamble,
        prefix,
        meta_group,
        meta_element,
        meta_length,
        transfer_syntax: String::from_utf8_lossy(transfer_syntax_bytes).to_string(),
    }))
}

fn parse_element(input: &[u8], explicit_vr: bool) -> IResult<&[u8], DicomElement> {
    let (input, group) = if explicit_vr { le_u16(input)? } else { be_u16(input)? };
    let (input, element) = if explicit_vr { le_u16(input)? } else { be_u16(input)? };
    
    let (input, vr, length) = if explicit_vr {
        let (input, vr_bytes) = take(2usize)(input)?;
        let vr = String::from_utf8_lossy(vr_bytes).to_string();
        
        let (input, length) = match vr.as_str() {
            "OB" | "OW" | "SQ" | "UN" => {
                let (input, _) = take(2usize)(input)?;
                let (input, length) = be_u32(input)?;
                (input, length)
            },
            _ => {
                let (input, length) = be_u16(input)?;
                (input, length as u32)
            }
        };
        (input, Some(vr), length)
    } else {
        let (input, length) = be_u32(input)?;
        (input, None, length)
    };
    
    let (input, value) = take(length)(input)?;
    
    Ok((input, DicomElement {
        group,
        element,
        vr,
        length,
        value: value.to_vec(),
    }))
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], DicomFile> {
    let (mut input, header) = parse_meta_header(input)?;
    let mut elements = Vec::new();
    
    let explicit_vr = header.transfer_syntax.contains("Explicit");
    
    while !input.is_empty() {
        match parse_element(input, explicit_vr) {
            Ok((remaining, element)) => {
                input = remaining;
                elements.push(element);
            },
            Err(_) => break,
        }
    }
    
    Ok((input, DicomFile { header, elements }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_dicom_file(&buffer) {
        Ok((_, dicom)) => println!("{:#?}", dicom),
        Err(e) => eprintln!("Error parsing DICOM: {:?}", e),
    }
}