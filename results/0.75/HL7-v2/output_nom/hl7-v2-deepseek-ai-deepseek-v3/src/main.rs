use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alphanumeric1, char, digit1, none_of},
    combinator::{map, opt, recognize},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

type HL7Result<'a, T> = IResult<&'a str, T>;

#[derive(Debug)]
struct HL7Message {
    segments: Vec<Segment>,
}

#[derive(Debug)]
struct Segment {
    id: String,
    fields: Vec<Field>,
}

#[derive(Debug)]
struct Field {
    components: Vec<Component>,
}

#[derive(Debug)]
struct Component {
    subcomponents: Vec<Subcomponent>,
}

#[derive(Debug)]
struct Subcomponent {
    value: String,
}

fn parse_subcomponent(input: &str) -> HL7Result<Subcomponent> {
    map(
        take_until("&"),
        |value: &str| Subcomponent {
            value: value.to_string(),
        },
    )(input)
}

fn parse_component(input: &str) -> HL7Result<Component> {
    map(
        separated_list1(tag("&"), parse_subcomponent),
        |subcomponents| Component { subcomponents },
    )(input)
}

fn parse_field(input: &str) -> HL7Result<Field> {
    map(
        separated_list1(tag("^"), parse_component),
        |components| Field { components },
    )(input)
}

fn parse_segment(input: &str) -> HL7Result<Segment> {
    map(
        tuple((
            alphanumeric1,
            tag("|"),
            separated_list1(tag("|"), parse_field),
        )),
        |(id, _, fields)| Segment {
            id: id.to_string(),
            fields,
        },
    )(input)
}

fn parse_hl7_message(input: &str) -> HL7Result<HL7Message> {
    map(
        many1(terminated(parse_segment, tag("\r"))),
        |segments| HL7Message { segments },
    )(input)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match parse_hl7_message(&contents) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse HL7 message: {:?}", e),
    }

    Ok(())
}