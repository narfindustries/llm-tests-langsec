Here's a basic example of how you might start writing a Rust program with Nom parser combinator to parse a DICOM file. This example won't cover the entire DICOM specification, as that's quite complex, but it will give you a foundation to build upon. DICOM files are structured in a way that could require a significant amount of detail to fully parse every aspect, including optional fields. Below is a simplified version focusing on reading the file header and some basic elements.

extern crate nom;
use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

// DICOM files have a specific preamble and prefix
fn parse_preamble(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let (input, preamble) = take(128u8)(input)?;
    let (input, _) = tag("DICM")(input)?;
    Ok((input, preamble))
}

// Example of a possible data element with VR (Value Representation)
fn parse_data_element(input: &[u8]) -> IResult<&[u8], (&[u8], u16, &[u8])> {
    let (input, tag) = take(2u8)(input)?; // Group and Element number
    let (input, vr) = take(2u8)(input)?; // Value Representation
    let (input, length) = le_u16(input)?; // Value Length
    let (input, value) = take(length)(input)?; // Value Field
    Ok((input, (tag, length, value)))
}

fn parse_dicom(input: &[u8]) -> IResult<&[u8], Vec<(&[u8], u16, &[u8])>> {
    let mut elements = Vec::new();
    let (mut input, _) = parse_preamble(input)?;

    while !input.is_empty() {
        match parse_data_element(input) {
            Ok((remaining, element)) => {
                elements.push(element);
                input = remaining;
            }
            Err(_) => break,
        }
    }

    Ok((input, elements))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <DICOM file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Error opening file {}: {}", filename, e);
            return;
        }
    };

    let mut buffer = Vec::new();
    if let Err(e) = file.read_to_end(&mut buffer) {
        eprintln!("Error reading file {}: {}", filename, e);
        return;
    }

    match parse_dicom(&buffer) {
        Ok((_, elements)) => {
            for (tag, length, value) in elements {
                println!(
                    "Tag: {:?}, Length: {}, Value: {:?}",
                    tag,
                    length,
                    &value[..std::cmp::min(20, value.len())] // Print first 20 bytes of value
                );
            }
        }
        Err(e) => eprintln!("Failed to parse DICOM file: {:?}", e),
    }
}

This code provides a basic framework for reading a DICOM file, parsing its preamble, and extracting data elements with a simple Value Representation (VR). The DICOM format is vast, and fully supporting it would require handling many VR types, possible implicit and explicit VRs, nested sequences, handling of pixel data, etc. For a complete implementation, you would need to refer to the DICOM standard documentation and likely build a more complex parser with error handling and support for all the DICOM data elements and their specificities.