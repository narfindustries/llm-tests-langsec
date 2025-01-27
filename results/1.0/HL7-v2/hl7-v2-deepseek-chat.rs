use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{char, line_ending, not_line_ending},
    combinator::{map, opt},
    multi::{many0, many1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct HL7Message {
    segments: Vec<HL7Segment>,
}

#[derive(Debug)]
struct HL7Segment {
    id: String,
    fields: Vec<HL7Field>,
}

#[derive(Debug)]
struct HL7Field {
    components: Vec<String>,
}

fn parse_component(input: &str) -> IResult<&str, String> {
    map(take_until("^"), |s: &str| s.to_string())(input)
}

fn parse_field(input: &str) -> IResult<&str, HL7Field> {
    let (input, components) = many0(terminated(parse_component, opt(char('^'))))(input)?;
    Ok((input, HL7Field { components }))
}

fn parse_segment(input: &str) -> IResult<&str, HL7Segment> {
    let (input, (id, fields)) = tuple((
        take_while1(|c: char| c.is_ascii_alphabetic()),
        many1(preceded(char('|'), parse_field)),
    ))(input)?;
    Ok((input, HL7Segment { id: id.to_string(), fields }))
}

fn parse_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, segments) = many1(terminated(parse_segment, line_ending))(input)?;
    Ok((input, HL7Message { segments }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Failed to read file");

    match parse_message(&contents) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse HL7 message: {:?}", e),
    }
}