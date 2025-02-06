use nom::{
    bytes::complete::{tag, take_while},
    character::complete::char,
    combinator::{opt, map_res},
    multi::{many0, separated_list0},
    IResult,
    branch::alt,
};
use std::env;
use std::fs;
use std::str;

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
    components: Vec<String>,
}

fn parse_delimiter(input: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((
        tag(b"|"),
        tag(b"^"),
        tag(b"~"),
        tag(b"\\"),
        tag(b"&")
    ))(input)
}

fn parse_field_value(input: &[u8]) -> IResult<&[u8], String> {
    map_res(
        take_while(|c: u8| c != b'|' && c != b'^' && c != b'~'),
        |bytes: &[u8]| String::from_utf8(bytes.to_vec())
    )(input)
}

fn parse_field(input: &[u8]) -> IResult<&[u8], Field> {
    let (input, components) = separated_list0(
        tag(b"^"),
        opt(parse_field_value)
    )(input)?;

    Ok((input, Field {
        components: components.into_iter().flatten().collect(),
    }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, segment_name) = take_while(|c: u8| c.is_ascii_uppercase())(input)?;
    let segment_name = str::from_utf8(segment_name).unwrap().to_string();

    let (input, _) = char('|')(input)?;

    let (input, fields) = separated_list0(
        tag(b"|"),
        parse_field
    )(input)?;

    Ok((input, Segment {
        name: segment_name,
        fields,
    }))
}

fn parse_hl7_message(input: &[u8]) -> IResult<&[u8], Hl7Message> {
    let (input, segments) = many0(parse_segment)(input)?;

    Ok((input, Hl7Message {
        segments,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
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
            std::process::exit(1)
        }
    }
}