use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{char, line_ending, not_line_ending, space0},
    combinator::{map, opt},
    multi::{many0, many1, separated_list0},
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
    components: Vec<String>,
}

fn parse_component(input: &str) -> IResult<&str, String> {
    map(take_until("^"), |s: &str| s.to_string())(input)
}

fn parse_field(input: &str) -> IResult<&str, HL7Field> {
    let (input, components) = separated_list0(char('^'), parse_component)(input)?;
    Ok((input, HL7Field { components }))
}

fn parse_segment(input: &str) -> IResult<&str, HL7Segment> {
    let (input, id) = take_while1(|c: char| c.is_ascii_alphabetic())(input)?;
    let (input, _) = char('|')(input)?;
    let (input, fields) = separated_list0(char('|'), opt(parse_field))(input)?;
    let (input, _) = line_ending(input)?;
    Ok((
        input,
        HL7Segment {
            id: id.to_string(),
            fields: fields.into_iter().flatten().collect(),
        },
    ))
}

fn parse_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, segments) = many1(parse_segment)(input)?;
    Ok((input, HL7Message { segments }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
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