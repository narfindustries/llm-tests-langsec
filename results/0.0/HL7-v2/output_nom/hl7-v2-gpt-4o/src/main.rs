use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{alphanumeric1, char, digit1, line_ending},
    combinator::{map_res, opt},
    multi::separated_list0,
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
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
    fields: Vec<String>,
}

fn parse_hl7_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, segments) = separated_list0(line_ending, parse_segment)(input)?;
    Ok((input, HL7Message { segments }))
}

fn parse_segment(input: &str) -> IResult<&str, Segment> {
    let (input, name) = alphanumeric1(input)?;
    let (input, fields) = separated_list0(char('|'), opt(alphanumeric1))(input)?;
    let fields = fields.into_iter().map(|f| f.unwrap_or("").to_string()).collect();
    Ok((input, Segment { name: name.to_string(), fields }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;

    match parse_hl7_message(&buffer) {
        Ok((_, message)) => {
            println!("{:?}", message);
        }
        Err(e) => {
            eprintln!("Failed to parse HL7 message: {:?}", e);
        }
    }

    Ok(())
}