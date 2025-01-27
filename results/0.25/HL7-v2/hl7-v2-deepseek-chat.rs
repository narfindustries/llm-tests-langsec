use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{char, line_ending, not_line_ending, space0},
    combinator::{map, opt},
    multi::{many0, many1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

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
    components: Vec<HL7Component>,
}

#[derive(Debug)]
struct HL7Component {
    subcomponents: Vec<String>,
}

fn parse_component(input: &str) -> IResult<&str, HL7Component> {
    let (input, subcomponents) = many0(terminated(
        take_while1(|c| c != '^' && c != '|' && c != '\r' && c != '\n'),
        opt(char('^')),
    ))(input)?;
    Ok((input, HL7Component { subcomponents }))
}

fn parse_field(input: &str) -> IResult<&str, HL7Field> {
    let (input, components) = many0(terminated(parse_component, opt(char('|'))))(input)?;
    Ok((input, HL7Field { components }))
}

fn parse_segment(input: &str) -> IResult<&str, HL7Segment> {
    let (input, (id, fields)) = tuple((
        take_while1(|c| c != '|' && c != '\r' && c != '\n'),
        preceded(
            char('|'),
            many0(terminated(parse_field, opt(char('|')))),
        ),
    ))(input)?;
    Ok((input, HL7Segment { id: id.to_string(), fields }))
}

fn parse_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, segments) = many1(terminated(parse_segment, line_ending))(input)?;
    Ok((input, HL7Message { segments }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <HL7 file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match parse_message(&contents) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse HL7 message: {:?}", e),
    }

    Ok(())
}