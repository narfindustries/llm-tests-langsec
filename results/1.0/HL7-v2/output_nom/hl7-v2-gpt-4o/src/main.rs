extern crate nom;

use nom::{
    bytes::complete::{tag, take_while},
    character::complete::{char, digit1},
    combinator::{map_res, opt},
    multi::separated_list0,
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

// HL7 Message Structure
#[derive(Debug)]
struct HL7Message {
    segments: Vec<Segment>,
}

#[derive(Debug)]
struct Segment {
    id: String,
    fields: Vec<String>,
}

fn parse_hl7_message(input: &[u8]) -> IResult<&[u8], HL7Message> {
    let (input, segments) = separated_list0(tag("\r"), parse_segment)(input)?;
    Ok((input, HL7Message { segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, (id, _, fields)) = tuple((
        take_while(|c: u8| c.is_ascii_alphabetic()),
        tag("|"),
        separated_list0(tag("|"), parse_field),
    ))(input)?;
    Ok((
        input,
        Segment {
            id: String::from_utf8(id.to_vec()).unwrap(),
            fields,
        },
    ))
}

fn parse_field(input: &[u8]) -> IResult<&[u8], String> {
    let (input, field) = take_while(|c: u8| c.is_ascii_graphic())(input)?;
    Ok((input, String::from_utf8(field.to_vec()).unwrap()))
}

fn read_file(file_path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(file_path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file_path>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    match read_file(file_path) {
        Ok(data) => match parse_hl7_message(&data) {
            Ok((_, message)) => {
                println!("{:#?}", message);
            }
            Err(e) => {
                eprintln!("Failed to parse HL7 message: {:?}", e);
            }
        },
        Err(e) => {
            eprintln!("Error reading file: {:?}", e);
        }
    }
}