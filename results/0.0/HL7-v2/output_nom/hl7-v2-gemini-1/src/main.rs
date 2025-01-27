use nom::{
    bytes::complete::{tag, take_while, take_until},
    character::complete::{alphanumeric1, char, line_ending},
    combinator::{map, map_res, opt, recognize},
    multi::many0,
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    IResult,
};
use std::env;
use std::fs;
use std::path::Path;

#[derive(Debug, PartialEq)]
struct Hl7Message {
    segments: Vec<Hl7Segment>,
}

#[derive(Debug, PartialEq)]
struct Hl7Segment {
    field_separator: char,
    component_separator: char,
    repetition_separator: char,
    escape_character: char,
    subcomponent_separator: char,
    fields: Vec<Hl7Field>,
}

#[derive(Debug, PartialEq)]
struct Hl7Field {
    components: Vec<Hl7Component>,
}


#[derive(Debug, PartialEq)]
struct Hl7Component {
    subcomponents: Vec<String>,
}


fn parse_hl7_message(input: &[u8]) -> IResult<&[u8], Hl7Message> {
    let mut input = input;
    let (input, msh_segment) = parse_hl7_segment(input)?;

    let (input, remaining_segments) = many0(parse_hl7_segment)(input)?;

    Ok((
        input,
        Hl7Message {
            segments: vec![msh_segment]
            .into_iter()
            .chain(remaining_segments)
            .collect(),
        },
    ))
}

fn parse_hl7_segment(input: &[u8]) -> IResult<&[u8], Hl7Segment> {
    let (input, field_separator) = map(char('|'), |c| c)(input)?;
    let (input, component_separator) = map(char('^'), |c| c)(input)?;
    let (input, repetition_separator) = map(char('&'), |c| c)(input)?;
    let (input, escape_character) = map(char('\\'), |c| c)(input)?;
    let (input, subcomponent_separator) = map(char('~'), |c| c)(input)?;

    let (input, fields) = many0(parse_hl7_field(field_separator, component_separator, repetition_separator, escape_character, subcomponent_separator))(input)?;

    Ok((
        input,
        Hl7Segment {
            field_separator,
            component_separator,
            repetition_separator,
            escape_character,
            subcomponent_separator,
            fields,
        },
    ))
}

fn parse_hl7_field(field_separator: char, component_separator: char, repetition_separator: char, escape_character: char, subcomponent_separator: char)(input: &[u8]) -> IResult<&[u8], Hl7Field> {
    let (input, components) = many0(parse_hl7_component(component_separator, repetition_separator, escape_character, subcomponent_separator))(input)?;
    Ok((input, Hl7Field { components }))
}

fn parse_hl7_component(component_separator: char, repetition_separator: char, escape_character: char, subcomponent_separator: char)(input: &[u8]) -> IResult<&[u8], Hl7Component> {
    let (input, subcomponents) = many0(parse_hl7_subcomponent(subcomponent_separator))(input)?;
    Ok((input, Hl7Component { subcomponents }))
}

fn parse_hl7_subcomponent(subcomponent_separator: char)(input: &[u8]) -> IResult<&[u8], String> {
    let (input, subcomponent) = take_until(subcomponent_separator.to_string().as_bytes())(input)?;
    Ok((input, String::from_utf8_lossy(subcomponent).to_string()))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let contents = fs::read(path).expect("Failed to read file");

    match parse_hl7_message(&contents) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Error parsing HL7 message: {:?}", e),
    }
}
