extern crate nom;

use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take_while},
    sequence::separated_pair,
    IResult,
    error::{Error, ErrorKind},
};

fn is_not_sep(c: u8) -> bool {
    c != b'\r' && c != b'|'
}

fn parse_field(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while(is_not_sep)(input)
}

fn parse_msh_segment(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8], &[u8], &[u8])> {
    let (input, (_msh, fields)) = separated_pair(tag("MSH"), tag("|"), take_while(|c| c != b'\r'))(input)?;
    let parts: Vec<&[u8]> = fields.split(|c| *c == b'|').collect();
    if parts.len() >= 7 {
        Ok((input, (parts[0], parts[1], parts[2], parts[3])))
    } else {
        Err(nom::Err::Error(Error::new(input, ErrorKind::LengthValue)))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    match parse_msh_segment(&buffer) {
        Ok((_remaining, (field_separator, encoding_characters, sending_app, sending_facility))) => {
            println!("Field Separator: {:?}", std::str::from_utf8(field_separator).unwrap());
            println!("Encoding Characters: {:?}", std::str::from_utf8(encoding_characters).unwrap());
            println!("Sending Application: {:?}", std::str::from_utf8(sending_app).unwrap());
            println!("Sending Facility: {:?}", std::str::from_utf8(sending_facility).unwrap());
        }
        Err(e) => {
            eprintln!("Failed to parse MSH segment: {:?}", e);
        }
    }
}