use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    number::complete::be_u16,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct DicomElement {
    group: u16,
    element: u16,
    vr: String,
    length: u32,
    value: Vec<u8>,
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, group) = be_u16(input)?;
    let (input, element) = be_u16(input)?;
    let (input, vr_bytes) = take(2usize)(input)?;
    let vr = String::from_utf8_lossy(vr_bytes).to_string();
    let (input, length) = be_u32(input)?;
    let (input, value) = take(length as usize)(input)?;
    Ok((
        input,
        DicomElement {
            group,
            element,
            vr,
            length,
            value: value.to_vec(),
        },
    ))
}


fn be_u32(input: &[u8]) -> IResult<&[u8], u32> {
    map(take(4usize), |bytes: &[u8]| {
        u32::from_be_bytes(bytes.try_into().unwrap())
    })(input)
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: dicom_parser <dicom_file>");
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let mut input = &buffer[..];
    loop {
        match parse_dicom_element(input) {
            Ok((remaining_input, element)) => {
                println!("Group: {:04X}, Element: {:04X}, VR: {}, Length: {}, Value: {:?}",
                         element.group, element.element, element.vr, element.length, element.value);
                input = remaining_input;
            }
            Err(nom::Err::Incomplete(_)) => {
                println!("Incomplete DICOM file");
                break;
            }
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                println!("Error parsing DICOM file: {:?}", e);
                break;
            }
        }
        if input.is_empty() {
            break;
        }
    }
}
