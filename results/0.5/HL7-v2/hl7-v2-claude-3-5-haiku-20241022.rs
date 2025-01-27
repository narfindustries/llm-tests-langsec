use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{char, digit1, multispace0, not_line_ending},
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
    subcomponents: Vec<String>,
}

fn parse_hl7_message(input: &[u8]) -> IResult<&[u8], HL7Message> {
    let (input, segments) = many1(parse_segment)(input)?;
    Ok((input, HL7Message { segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, name) = take_while1(|c: u8| c.is_ascii_uppercase())(input)?;
    let (input, _) = char('|')(input)?;
    let (input, fields) = separated_list0(char('|'), parse_field)(input)?;
    let (input, _) = opt(char('\r'))(input)?;
    let (input, _) = opt(char('\n'))(input)?;

    Ok((input, Segment {
        name: str::from_utf8(name).unwrap().to_string(),
        fields,
    }))
}

fn parse_field(input: &[u8]) -> IResult<&[u8], Field> {
    let (input, components) = separated_list0(char('^'), parse_component)(input)?;
    Ok((input, Field { components }))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, value) = take_while(|c: u8| c != b'^' && c != b'|' && c != b'\r' && c != b'\n')(input)?;
    let (input, subcomponents) = opt(preceded(char('^'), separated_list0(char('&'), parse_subcomponent)))(input)?;

    Ok((input, Component {
        value: str::from_utf8(value).unwrap().to_string(),
        subcomponents: subcomponents.unwrap_or_default().iter().map(|s| s.to_string()).collect(),
    }))
}

fn parse_subcomponent(input: &[u8]) -> IResult<&[u8], &str> {
    let (input, subcomp) = take_while(|c: u8| c != b'&' && c != b'^' && c != b'|' && c != b'\r' && c != b'\n')(input)?;
    Ok((input, str::from_utf8(subcomp).unwrap()))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
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