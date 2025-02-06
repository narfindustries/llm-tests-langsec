use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct DicomElement {
    tag_group: u16,
    tag_element: u16,
    vr: String,
    length: u32,
    value: Vec<u8>,
}

fn parse_dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, tag_group) = le_u16(input)?;
    let (input, tag_element) = le_u16(input)?;
    let (input, vr) = take(2usize)(input)?;
    let vr = String::from_utf8(vr.to_vec()).unwrap();
    let (input, length) = if ["OB", "OW", "OF", "SQ", "UT", "UN"].contains(&vr.as_str()) {
        let (input, _) = take(2usize)(input)?; // Reserved bytes
        let (input, length) = le_u32(input)?;
        (input, length)
    } else {
        let (input, length) = le_u16(input)?;
        (input, length as u32)
    };
    let (input, value) = take(length)(input)?;
    Ok((
        input,
        DicomElement {
            tag_group,
            tag_element,
            vr,
            length,
            value: value.to_vec(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <DICOM file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    let mut offset = 0;
    while offset < data.len() {
        match parse_dicom_element(&data[offset..]) {
            Ok((remaining, element)) => {
                println!("{:?}", element);
                offset = data.len() - remaining.len();
            }
            Err(e) => {
                println!("Error parsing DICOM: {:?}", e);
                break;
            }
        }
    }

    Ok(())
}