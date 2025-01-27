use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{char, none_of},
    combinator::{map, opt},
    multi::{many0, many1},
    sequence::{delimited, preceded, tuple},
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
    let (input, segment_type) = take_while1(|c: char| c.is_alphabetic())(input)?;
    let (input, _) = char('|')(input)?;
    let (input, fields) = many0(parse_field)(input)?;
    let (input, _) = opt(tag("\r\n"))(input)?;
    Ok((
        input,
        Segment {
            segment_type: segment_type.to_string(),
            fields,
        },
    ))
}

fn parse_field(input: &str) -> IResult<&str, Field> {
    let (input, components) = alt((
        map(take_until("|"), |s: &str| {
            vec![Component {
                subcomponents: vec![s.to_string()],
            }]
        }),
        many1(parse_component),
    ))(input)?;
    let (input, _) = char('|')(input)?;
    Ok((input, Field { components }))
}

fn parse_component(input: &str) -> IResult<&str, Component> {
    let (input, subcomponents) = many1(delimited(
        opt(char('^')),
        map(take_while1(|c| c != '^' && c != '|'), |s: &str| s.to_string()),
        opt(char('^')),
    ))(input)?;
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

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Failed to read file");

    match parse_hl7_message(&contents) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse HL7 message: {:?}", e),
    }
}