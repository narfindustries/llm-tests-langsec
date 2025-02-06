use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{char, digit1},
    combinator::{opt, map_res},
    multi::{many0, many1},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs;
use std::str::from_utf8;

#[derive(Debug)]
struct Hl7Message {
    segments: Vec<Segment>,
}

#[derive(Debug)]
struct Segment {
    name: String,
    fields: Vec<Field>,
}

#[derive(Debug)]
struct Field {
    value: Option<String>,
    components: Vec<Component>,
}

#[derive(Debug)]
struct Component {
    value: Option<String>,
    subcomponents: Vec<Subcomponent>,
}

#[derive(Debug)]
struct Subcomponent {
    value: Option<String>,
}

fn parse_hl7_message(input: &[u8]) -> IResult<&[u8], Hl7Message> {
    let (input, segments) = many1(parse_segment)(input)?;
    Ok((input, Hl7Message { segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, name) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, fields) = many1(parse_field)(input)?;
    let name_str = from_utf8(name).unwrap().to_string();
    Ok((input, Segment { name: name_str, fields }))
}

fn parse_field(input: &[u8]) -> IResult<&[u8], Field> {
    let (input, value) = opt(take_until("|"))(input)?;
    let (input, _) = opt(char('|'))(input)?;
    let (input, components) = many0(parse_component)(input)?;
    
    let value_str = value.map(|v| from_utf8(v).unwrap().to_string());
    Ok((input, Field { value: value_str, components }))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, value) = opt(take_until("^"))(input)?;
    let (input, _) = opt(char('^'))(input)?;
    let (input, subcomponents) = many0(parse_subcomponent)(input)?;
    
    let value_str = value.map(|v| from_utf8(v).unwrap().to_string());
    Ok((input, Component { value: value_str, subcomponents }))
}

fn parse_subcomponent(input: &[u8]) -> IResult<&[u8], Subcomponent> {
    let (input, value) = opt(take_until("&"))(input)?;
    let (input, _) = opt(char('&'))(input)?;
    
    let value_str = value.map(|v| from_utf8(v).unwrap().to_string());
    Ok((input, Subcomponent { value: value_str }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let contents = fs::read(filename).expect("Unable to read file");

    match parse_hl7_message(&contents) {
        Ok((_, message)) => {
            println!("Parsed HL7 Message: {:?}", message);
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}