use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{char, none_of},
    combinator::{map, opt},
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};
use std::{env, fs::File, io::Read};

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
    let (input, fields) = terminated(many0(parse_field), char('\r'))(input)?;
    
    Ok((
        input,
        Segment {
            segment_type: segment_type.to_string(),
            fields,
        },
    ))
}

fn parse_field(input: &str) -> IResult<&str, Field> {
    let (input, components) = terminated(many0(parse_component), char('|'))(input)?;
    Ok((input, Field { components }))
}

fn parse_component(input: &str) -> IResult<&str, Component> {
    let (input, subcomponents) = many0(terminated(
        map(
            many1(none_of("^~\\|")),
            |chars: Vec<char>| chars.into_iter().collect::<String>(),
        ),
        opt(char('^')),
    ))(input)?;
    
    Ok((
        input,
        Component { subcomponents },
    ))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
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