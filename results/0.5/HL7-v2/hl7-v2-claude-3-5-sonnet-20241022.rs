use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{char, none_of},
    combinator::opt,
    multi::{many0, many1},
    sequence::{delimited, tuple},
    IResult,
};
use std::{env, fs, str};

#[derive(Debug)]
struct HL7Message {
    segments: Vec<Segment>,
}

#[derive(Debug)]
struct Segment {
    segment_type: String,
    fields: Vec<Field>,
}

#[derive(Debug)]
struct Field {
    components: Vec<Component>,
}

#[derive(Debug)]
struct Component {
    subcomponents: Vec<String>,
}

fn parse_subcomponent(input: &[u8]) -> IResult<&[u8], String> {
    let (input, content) = many0(none_of("^~\\|&"))(input)?;
    Ok((input, content.into_iter().collect()))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Component> {
    let (input, subcomponents) = many1(delimited(
        opt(char('^')),
        parse_subcomponent,
        opt(char('^')),
    ))(input)?;
    Ok((input, Component { subcomponents }))
}

fn parse_field(input: &[u8]) -> IResult<&[u8], Field> {
    let (input, components) = many1(delimited(
        opt(char('|')),
        parse_component,
        opt(char('|')),
    ))(input)?;
    Ok((input, Field { components }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, segment_type) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, fields) = many0(parse_field)(input)?;
    let (input, _) = opt(char('\r'))(input)?;
    let (input, _) = char('\n')(input)?;
    
    Ok((input, Segment {
        segment_type: str::from_utf8(segment_type).unwrap().to_string(),
        fields,
    }))
}

fn parse_hl7_message(input: &[u8]) -> IResult<&[u8], HL7Message> {
    let (input, _) = tag("\x0B")(input)?;
    let (input, segments) = many1(parse_segment)(input)?;
    let (input, _) = tag("\x1C\r\n")(input)?;
    
    Ok((input, HL7Message { segments }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let contents = fs::read(filename).expect("Failed to read file");

    match parse_hl7_message(&contents) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse HL7 message: {:?}", e),
    }
}