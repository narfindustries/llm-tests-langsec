use nom::{
    character::complete::{alphanumeric1, char, line_ending},
    combinator::{map, map_res, opt, recognize},
    multi::separated_list1,
    sequence::terminated,
    IResult,
};
use std::env;
use std::fs::read;
use std::str;

#[derive(Debug)]
struct Hl7Message {
    segments: Vec<Hl7Segment>,
}

#[derive(Debug)]
struct Hl7Segment {
    name: String,
    fields: Vec<String>,
}

fn field(input: &[u8]) -> IResult<&[u8], String> {
    let parser = recognize(many1(anychar));
    map_res(terminated(parser, char('|')), str::from_utf8)
        .map(|s| s.to_string())(input)
}

fn escape_char(input: &[u8]) -> IResult<&[u8], char> {
    let (input, c) = char('\\')(input)?;
    let (input, escaped) = match c {
        '|' => Ok((input, '|')),
        '\\' => Ok((input, '\\')),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Char))),
    }?;
    Ok((input, escaped))
}

fn anychar(input: &[u8]) -> IResult<&[u8], char> {
    let (input, c) = nom::character::complete::anychar(input)?;
    Ok((input, c))
}

fn segment(input: &[u8]) -> IResult<&[u8], Hl7Segment> {
    let (input, name) = terminated(
        recognize(many1(alt((alphanumeric1, map(escape_char, |c| c.to_string().into_bytes()))))),
        char('|'),
    )(input)?;
    let name_str = str::from_utf8(name).unwrap();

    let (input, fields) = separated_list1(char('|'), field)(input)?;
    let (input, _) = opt(line_ending)(input)?;
    Ok((
        input,
        Hl7Segment {
            name: name_str.to_string(),
            fields,
        },
    ))
}

fn message(input: &[u8]) -> IResult<&[u8], Hl7Message> {
    let (input, segments) = separated_list1(line_ending, segment)(input)?;
    Ok((input, Hl7Message { segments }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <filename>", args[0]);
        return;
    }
    let filename = &args[1];
    let data = read(filename).expect("Failed to read file");
    match message(&data) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => println!("Error parsing HL7 message: {:?}", e),
    }
}

use nom::branch::alt;
use nom::multi::many1;
