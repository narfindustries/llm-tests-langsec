use nom::{
    bytes::complete::{tag, take},
    combinator::map,
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
    file_meta_info_group_length: u32,
    file_version: String,
    media_storage_sop_class_uid: String,
    media_storage_sop_instance_uid: String,
    transfer_syntax_uid: String,
    implementation_class_uid: String,
}

#[derive(Debug)]
struct DataSet {
    elements: Vec<DataElement>,
}

#[derive(Debug)]
struct DataElement {
    tag: Tag,
    vr: Option<String>,
    length: u32,
    value: Vec<u8>,
}

#[derive(Debug)]
struct Tag(u16, u16);

fn parse_preamble(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(take(128usize), |bytes: &[u8]| bytes.to_vec())(input)
}

fn parse_dicom_prefix(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(tag("DICM"), |t: &[u8]| t.to_vec())(input)
}

fn parse_tag(input: &[u8]) -> IResult<&[u8], Tag> {
    map(tuple((be_u16, be_u16)), |(group, element)| Tag(group, element))(input)
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], Option<String>> {
    map(take(2usize), |vr: &[u8]| {
        Some(String::from_utf8_lossy(vr).to_string())
    })(input)
}

fn parse_length(input: &[u8], explicit_vr: bool, vr: Option<String>) -> IResult<&[u8], u32> {
    if explicit_vr {
        match vr.as_deref() {
            Some("OB") | Some("OW") | Some("SQ") | Some("UN") => {
                let (input, _) = take(2usize)(input)?;
                be_u32(input)
            }
            _ => map(le_u16, |x| x as u32)(input),
        }
    } else {
        le_u32(input)
    }
}

fn parse_value(input: &[u8], length: u32) -> IResult<&[u8], Vec<u8>> {
    map(take(length as usize), |v: &[u8]| v.to_vec())(input)
}

fn parse_data_element(input: &[u8], explicit_vr: bool) -> IResult<&[u8], DataElement> {
    let (input, tag) = parse_tag(input)?;
    let (input, vr) = if explicit_vr {
        parse_vr(input)?
    } else {
        (input, None)
    };
    let (input, length) = parse_length(input, explicit_vr, vr.clone())?;
    let (input, value) = parse_value(input, length)?;
    
    Ok((input, DataElement {
        tag,
        vr,
        length,
        value,
    }))
}

fn parse_dicom(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, preamble) = parse_preamble(input)?;
    let (input, prefix) = parse_dicom_prefix(input)?;
    
    // Parse meta information header
    let mut current_input = input;
    let mut elements = Vec::new();
    
    while !current_input.is_empty() {
        let (remaining, element) = parse_data_element(current_input, true)?;
        elements.push(element);
        current_input = remaining;
        
        // Break after meta information group (0002,xxxx)
        if elements.last().unwrap().tag.0 != 0x0002 {
            break;
        }
    }
    
    // Continue parsing dataset
    while !current_input.is_empty() {
        let (remaining, element) = parse_data_element(current_input, true)?;
        elements.push(element);
        current_input = remaining;
    }
    
    Ok((current_input, DicomHeader {
        preamble,
        prefix,
        meta_info: MetaInformation {
            file_meta_info_group_length: 0,
            file_version: String::new(),
            media_storage_sop_class_uid: String::new(),
            media_storage_sop_instance_uid: String::new(),
            transfer_syntax_uid: String::new(),
            implementation_class_uid: String::new(),
        },
        dataset: DataSet { elements },
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dicom(&buffer) {
        Ok((_, dicom)) => println!("{:#?}", dicom),
        Err(e) => eprintln!("Error parsing DICOM: {:?}", e),
    }

    Ok(())
}