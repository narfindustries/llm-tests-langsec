use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit1, space0, space1},
    combinator::{map, opt},
    multi::{many0, many1, separated_list0},
    sequence::{preceded, separated_pair, terminated, tuple},
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
    value: String,
    repetitions: Vec<Repetition>,
}

#[derive(Debug)]
struct Repetition {
    components: Vec<Component>,
}

#[derive(Debug)]
struct Component {
    subcomponents: Vec<String>,
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
            take_while1(|c| c.is_ascii_uppercase()),
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
        tuple((
            opt(take_while(|c| c != b'|' && c != b'^' && c != b'~')),
            many0(preceded(char('^'), parse_component)),
            many0(preceded(char('~'), parse_repetition))
        )),
        |(value, components, repetitions)| Field {
            value: value.map(|v| str::from_utf8(v).unwrap().to_string()).unwrap_or_default(),
            repetitions: if components.is_empty() && repetitions.is_empty() {
                vec![]
            } else {
                let mut reps = vec![];
                if !components.is_empty() {
                    reps.push(Repetition { components });
                }
                reps.extend(repetitions);
                reps
            },
        }
    )(input)
}

fn parse_repetition(input: &[u8]) -> IResult<&[u8], Repetition> {
    map(
        separated_list0(char('^'), parse_component),
        |components| Repetition { components }
    )(input)
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    map(
        separated_list0(char('&'), opt(take_while(|c| c != b'|' && c != b'^' && c != b'~' && c != b'&'))),
        |subcomponents| Component {
            subcomponents: subcomponents
                .into_iter()
                .map(|sc| sc.map(|v| str::from_utf8(v).unwrap().to_string()).unwrap_or_default())
                .collect(),
        }
    )(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        std::process::exit(1);
    }

    let file_contents = fs::read(&args[1])?;
    
    match parse_hl7_message(&file_contents) {
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