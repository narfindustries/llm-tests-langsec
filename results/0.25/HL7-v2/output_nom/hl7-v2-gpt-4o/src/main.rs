use nom::{
    bytes::complete::{tag, take_until, take_while},
    character::complete::{char, digit1, line_ending, not_line_ending},
    combinator::{map_res, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, terminated, tuple},
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

fn is_not_pipe(c: char) -> bool {
    c != '|'
}

fn parse_field(input: &str) -> IResult<&str, String> {
    map_res(take_while(is_not_pipe), |s: &str| Ok(s.to_string()))(input)
}

fn parse_segment(input: &str) -> IResult<&str, Segment> {
    let (input, name) = map_res(take_while(|c: char| c.is_alphabetic()), |s: &str| {
        Ok(s.to_string())
    })(input)?;
    let (input, fields) = separated_list0(char('|'), parse_field)(input)?;
    let (input, _) = opt(line_ending)(input)?;
    Ok((input, Segment { name, fields }))
}

fn parse_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, segments) = many0(parse_segment)(input)?;
    Ok((input, HL7Message { segments }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;

    match parse_message(&buffer) {
        Ok((_, message)) => {
            println!("{:#?}", message);
        }
        Err(e) => {
            eprintln!("Failed to parse HL7 message: {:?}", e);
        }
    }

    Ok(())
}
