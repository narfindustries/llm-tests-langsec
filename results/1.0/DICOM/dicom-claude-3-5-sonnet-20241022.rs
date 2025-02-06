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
    meta_group_length: u32,
    file_meta_info_version: Vec<u8>,
    media_storage_sop_class_uid: String,
    media_storage_sop_instance_uid: String,
    transfer_syntax_uid: String,
    implementation_class_uid: String,
    implementation_version_name: Option<String>,
}

#[derive(Debug)]
struct DicomDataSet {
    header: DicomHeader,
    elements: Vec<DicomElement>,
}

#[derive(Debug)]
struct DicomElement {
    tag: (u16, u16),
    vr: Option<String>,
    length: u32,
    value: Vec<u8>,
}

fn parse_dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = take(128u8)(input)?;
    let (input, prefix) = tag(b"DICM")(input)?;
    let (input, meta_group_length) = be_u32(input)?;
    let (input, file_meta_info_version) = take(2u8)(input)?;
    
    // Parse other meta elements - simplified for brevity
    Ok((
        input,
        DicomHeader {
            preamble: preamble.to_vec(),
            prefix: prefix.to_vec(),
            meta_group_length,
            file_meta_info_version: file_meta_info_version.to_vec(),
            media_storage_sop_class_uid: String::new(),
            media_storage_sop_instance_uid: String::new(),
            transfer_syntax_uid: String::new(),
            implementation_class_uid: String::new(),
            implementation_version_name: None,
        },
    ))
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, group) = be_u16(input)?;
    let (input, element) = be_u16(input)?;
    let (input, vr_bytes) = take(2u8)(input)?;
    let vr = String::from_utf8(vr_bytes.to_vec()).ok();
    
    let (input, length) = be_u32(input)?;
    let (input, value) = take(length)(input)?;

    Ok((
        input,
        DicomElement {
            tag: (group, element),
            vr,
            length,
            value: value.to_vec(),
        },
    ))
}

fn parse_dicom(input: &[u8]) -> IResult<&[u8], DicomDataSet> {
    let (input, header) = parse_dicom_header(input)?;
    let mut elements = Vec::new();
    let mut remaining = input;

    while !remaining.is_empty() {
        match parse_dicom_element(remaining) {
            Ok((rest, element)) => {
                elements.push(element);
                remaining = rest;
            }
            Err(_) => break,
        }
    }

    Ok((
        remaining,
        DicomDataSet {
            header,
            elements,
        },
    ))
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

    match parse_dicom(&buffer) {
        Ok((_, dataset)) => println!("{:?}", dataset),
        Err(e) => eprintln!("Failed to parse DICOM: {:?}", e),
    }
}