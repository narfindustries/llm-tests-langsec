use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{char, none_of},
    combinator::{map, opt},
    multi::{many0, many1},
    sequence::{delimited, tuple},
    IResult,
};
use std::{env, fs, path::Path};

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

fn parse_hl7_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, segments) = many1(parse_segment)(input)?;
    Ok((input, HL7Message { segments }))
}

fn parse_segment(input: &str) -> IResult<&str, Segment> {
    let (input, segment_type) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, fields) = many0(parse_field)(input)?;
    let (input, _) = opt(char('\r'))(input)?;
    let (input, _) = char('\n')(input)?;
    
    Ok((
        input,
        Segment {
            segment_type: segment_type.to_string(),
            fields,
        },
    ))
}

fn parse_field(input: &str) -> IResult<&str, Field> {
    let (input, components) = delimited(
        opt(char('|')),
        many0(parse_component),
        opt(char('|')),
    )(input)?;
    
    Ok((input, Field { components }))
}

fn parse_component(input: &str) -> IResult<&str, Component> {
    let (input, subcomponents) = delimited(
        opt(char('^')),
        many0(map(
            many0(none_of("|^&\r\n")),
            |chars: Vec<char>| chars.into_iter().collect::<String>(),
        )),
        opt(char('^')),
    )(input)?;
    
    Ok((
        input,
        Component {
            subcomponents,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        std::process::exit(1);
    }

    let file_path = Path::new(&args[1]);
    let content = fs::read_to_string(file_path).expect("Failed to read file");

    match parse_hl7_message(&content) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse HL7 message: {:?}", e),
    }
}