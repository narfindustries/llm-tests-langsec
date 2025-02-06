use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{char, not_line_ending},
    combinator::{opt, map},
    multi::{many0, separated_list0},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs;
use std::error::Error;

#[derive(Debug)]
struct HL7Message {
    segments: Vec<Segment>,
}

#[derive(Debug)]
struct Segment {
    name: String,
    fields: Vec<Field>,
}

#[derive(Debug)]
struct Field {
    components: Vec<String>,
}

fn parse_field(input: &[u8]) -> IResult<&[u8], Field> {
    map(
        separated_list0(char('^'), take_until("|~")),
        |components| Field {
            components: components.iter().map(|&c| String::from_utf8_lossy(c).to_string()).collect()
        }
    )(input)
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, name) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    
    let (input, fields) = separated_list0(char('|'), parse_field)(input)?;

    Ok((input, Segment {
        name: String::from_utf8_lossy(name).to_string(),
        fields,
    }))
}

fn parse_hl7_message(input: &[u8]) -> IResult<&[u8], HL7Message> {
    map(
        many0(parse_segment),
        |segments| HL7Message { segments }
    )(input)
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let contents = fs::read(filename)?;

    match parse_hl7_message(&contents) {
        Ok((_, message)) => {
            println!("Parsed HL7 Message: {:?}", message);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            Err("Failed to parse HL7 message".into())
        }
    }
}