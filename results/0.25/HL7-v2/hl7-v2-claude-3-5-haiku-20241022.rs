use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::complete::{alphanumeric1, char, digit1, multispace0, not_line_ending},
    combinator::{map, opt},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::env;
use std::fs;
use std::str;

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
    components: Vec<Component>,
}

#[derive(Debug)]
struct Component {
    value: String,
}

fn parse_hl7_message(input: &[u8]) -> IResult<&[u8], HL7Message> {
    map(
        many1(parse_segment),
        |segments| HL7Message { segments }
    )(input)
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    map(
        tuple((
            alphanumeric1,
            char('|'),
            separated_list0(char('|'), parse_field)
        )),
        |(name, _, fields)| Segment {
            name: str::from_utf8(name).unwrap().to_string(),
            fields,
        }
    )(input)
}

fn parse_field(input: &[u8]) -> IResult<&[u8], Field> {
    map(
        separated_list0(char('^'), parse_component),
        |components| Field { components }
    )(input)
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    map(
        alt((
            take_while(|c| c != b'|' && c != b'^'),
            tag(b"")
        )),
        |value| Component {
            value: str::from_utf8(value).unwrap().to_string(),
        }
    )(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input = fs::read(&args[1])?;
    match parse_hl7_message(&input) {
        Ok((_, message)) => {
            println!("Parsed HL7 Message: {:?}", message);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}