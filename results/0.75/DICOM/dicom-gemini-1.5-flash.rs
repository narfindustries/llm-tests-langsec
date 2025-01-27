use nom::{
    bytes::complete::{tag, take, take_while, take_while1},
    combinator::{map, map_res, opt, recognize},
    number::complete::{be_u16, be_u32, be_i32},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct DicomHeader {
    prefix: Vec<u8>,
    group_length: u32,
    // Add other fields as needed based on the DICOM specification
}

fn dicom_prefix(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    take(128usize)(input)
}


fn dicom_group_length(input: &[u8]) -> IResult<&[u8], u32> {
    be_u32(input)
}

fn dicom_header(input: &[u8]) -> IResult<&[u8], DicomHeader> {
    let (input, prefix) = dicom_prefix(input)?;
    let (input, group_length) = dicom_group_length(input)?;

    Ok((
        input,
        DicomHeader {
            prefix,
            group_length,
        },
    ))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <DICOM_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match dicom_header(&buffer) {
        Ok((remaining, header)) => {
            println!("DICOM Header: {:?}", header);
            println!("Remaining bytes: {} ", remaining.len());
            //Further parsing of the remaining data based on the DICOM specification
        }
        Err(e) => {
            eprintln!("Failed to parse DICOM header: {:?}", e);
            std::process::exit(1);
        }
    }
}
