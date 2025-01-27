use nom::{
    bytes::complete::{take_until, take_while},
    character::complete::{char, digit1},
    combinator::opt,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct HL7Message {
    segments: Vec<Segment>,
}

#[derive(Debug)]
struct Segment {
    name: String,
    fields: Vec<Option<String>>,
}

fn parse_hl7_message(input: &[u8]) -> IResult<&[u8], HL7Message> {
    let (input, segments) = nom::multi::separated_list1(char('\n'), parse_segment)(input)?;
    Ok((input, HL7Message { segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, name) = terminated(take_while(|c: u8| c.is_ascii_uppercase()), char('|'))(input)?;
    let (input, fields) = nom::multi::separated_list0(char('|'), opt(parse_field))(input)?;
    Ok((
        input,
        Segment {
            name: String::from_utf8_lossy(name).to_string(),
            fields,
        },
    ))
}

fn parse_field(input: &[u8]) -> IResult<&[u8], String> {
    let (input, field) = take_until("|")(input)?;
    Ok((input, String::from_utf8_lossy(field).to_string()))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_hl7_message(&buffer) {
        Ok((_, hl7_message)) => {
            println!("{:#?}", hl7_message);
        }
        Err(e) => {
            eprintln!("Failed to parse HL7 message: {:?}", e);
        }
    }

    Ok(())
}