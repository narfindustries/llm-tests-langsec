use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    number::complete::be_u16,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct DicomElement {
    group: u16,
    element: u16,
    vr: String,
    value_length: u32,
    value: Vec<u8>,
}

fn dicom_element(input: &[u8]) -> IResult<&[u8], DicomElement> {
    let (input, group) = be_u16(input)?;
    let (input, element) = be_u16(input)?;
    let (input, vr_bytes) = take(2usize)(input)?;
    let vr = String::from_utf8_lossy(vr_bytes).to_string();
    let (input, value_length) = be_u32(input)?;
    let (input, value) = take(value_length as usize)(input)?;
    Ok((
        input,
        DicomElement {
            group,
            element,
            vr,
            value_length,
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
        eprintln!("Usage: {} <dicom_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file {}: {}", filename, err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file {}: {}", filename, err);
            return;
        }
    };

    let result = nom::multi::many0(dicom_element)(&buffer);

    match result {
        Ok((_, elements)) => {
            for element in elements {
                println!("{:?}", element);
            }
        }
        Err(err) => {
            eprintln!("Error parsing DICOM file: {:?}", err);
        }
    }
}
