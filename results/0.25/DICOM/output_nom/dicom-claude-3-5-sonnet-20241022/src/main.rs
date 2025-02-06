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
    meta_info: MetaInformation,
    dataset: DataSet,
}

#[derive(Debug)]
struct MetaInformation {
    group_length: u32,
    file_meta_information_version: Vec<u8>,
    media_storage_sop_class_uid: String,
    media_storage_sop_instance_uid: String,
    transfer_syntax_uid: String,
    implementation_class_uid: String,
    implementation_version_name: Option<String>,
}

#[derive(Debug)]
struct DataSet {
    elements: Vec<DataElement>,
}

#[derive(Debug)]
struct DataElement {
    tag: (u16, u16),
    vr: Option<String>,
    length: u32,
    value: Vec<u8>,
}

fn parse_dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = take(128usize)(input)?;
    let (input, prefix) = tag(b"DICM")(input)?;
    let (input, meta_info) = parse_meta_information(input)?;
    let (input, dataset) = parse_dataset(input)?;

    Ok((
        input,
        DicomHeader {
            preamble: preamble.to_vec(),
            prefix: prefix.to_vec(),
            meta_info,
            dataset,
        },
    ))
}

fn parse_meta_information(input: &[u8]) -> IResult<&[u8], MetaInformation> {
    let (input, (group, element)) = tuple((be_u16, be_u16))(input)?;
    let (input, length) = be_u32(input)?;
    let (input, group_length) = be_u32(input)?;

    // Parse other meta information elements
    // This is a simplified version; actual implementation would need to handle all possible meta information elements

    Ok((
        input,
        MetaInformation {
            group_length,
            file_meta_information_version: vec![],
            media_storage_sop_class_uid: String::new(),
            media_storage_sop_instance_uid: String::new(),
            transfer_syntax_uid: String::new(),
            implementation_class_uid: String::new(),
            implementation_version_name: None,
        },
    ))
}

fn parse_dataset(input: &[u8]) -> IResult<&[u8], DataSet> {
    let mut elements = Vec::new();
    let mut remaining = input;

    while !remaining.is_empty() {
        let (input, element) = parse_data_element(remaining)?;
        elements.push(element);
        remaining = input;
    }

    Ok((remaining, DataSet { elements }))
}

fn parse_data_element(input: &[u8]) -> IResult<&[u8], DataElement> {
    let (input, tag) = tuple((be_u16, be_u16))(input)?;
    let (input, vr) = if is_explicit_vr(tag) {
        let (input, vr_chars) = take(2usize)(input)?;
        (input, Some(String::from_utf8_lossy(vr_chars).to_string()))
    } else {
        (input, None)
    };

    let (input, length) = match &vr {
        Some(vr) if ["OB", "OW", "OF", "SQ", "UT", "UN"].contains(&vr.as_str()) => {
            let (input, _) = take(2usize)(input)?;
            be_u32(input)?
        }
        Some(_) => {
            let (input, len) = be_u16(input)?;
            (input, len as u32)
        }
        None => be_u32(input)?,
    };

    let (input, value) = take(length as usize)(input)?;

    Ok((
        input,
        DataElement {
            tag,
            vr,
            length,
            value: value.to_vec(),
        },
    ))
}

fn is_explicit_vr(tag: (u16, u16)) -> bool {
    // Implement logic to determine if element should have explicit VR
    true
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dicom-file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dicom_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => eprintln!("Error parsing DICOM file: {:?}", e),
    }

    Ok(())
}