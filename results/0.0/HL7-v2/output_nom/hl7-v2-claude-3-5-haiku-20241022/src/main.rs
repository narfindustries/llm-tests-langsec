use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while, take_while1},
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
    value: String,
    repetitions: Vec<String>,
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
    let name = str::from_utf8(name).unwrap().to_string();
    
    let (input, _) = char('|')(input)?;
    
    let (input, fields) = separated_list0(char('|'), parse_field)(input)?;
    
    let (input, _) = opt(char('\r'))(input)?;
    let (input, _) = opt(char('\n'))(input)?;
    
    Ok((input, Segment { name, fields }))
}

fn parse_field(input: &[u8]) -> IResult<&[u8], Field> {
    let (input, value) = take_while(|c: u8| c != b'|' && c != b'\r' && c != b'\n')(input)?;
    let value = str::from_utf8(value).unwrap_or("").to_string();
    
    let (input, repetitions) = many0(preceded(char('^'), 
        take_while(|c: u8| c != b'|' && c != b'^' && c != b'\r' && c != b'\n')
    ))(input)?;
    let repetitions = repetitions.iter()
        .map(|&r| str::from_utf8(r).unwrap_or("").to_string())
        .collect();
    
    let (input, components) = many0(preceded(char('&'), parse_component))(input)?;
    
    Ok((input, Field { value, repetitions, components }))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, value) = take_while(|c: u8| c != b'&' && c != b'|' && c != b'\r' && c != b'\n')(input)?;
    let value = str::from_utf8(value).unwrap_or("").to_string();
    
    let (input, subcomponents) = many0(preceded(char('^'), 
        take_while(|c: u8| c != b'&' && c != b'|' && c != b'^' && c != b'\r' && c != b'\n')
    ))(input)?;
    let subcomponents = subcomponents.iter()
        .map(|&s| str::from_utf8(s).unwrap_or("").to_string())
        .collect();
    
    Ok((input, Component { value, subcomponents }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
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
            std::process::exit(1);
        }
    }
}