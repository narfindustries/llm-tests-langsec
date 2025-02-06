use nom::{
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, alphanumeric1, char, digit1, line_ending},
    combinator::{map, map_res, opt, recognize},
    multi::separated_list1,
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult,
};
use std::env;
use std::fs;
use std::path::Path;

#[derive(Debug)]
struct Hl7Message {
    segments: Vec<Hl7Segment>,
}

#[derive(Debug)]
struct Hl7Segment {
    segment_id: String,
    fields: Vec<String>,
}

fn hl7_field(input: &str) -> IResult<&str, String> {
    let parser = recognize(many1(
        alt((alphanumeric1, tag("|"), tag("^"), tag("&"), tag("~"), tag("\\"))),
    ));
    map(parser, |s| s.to_string())(input)
}

fn hl7_segment(input: &str) -> IResult<&str, Hl7Segment> {
    let (input, segment_id) = map(alpha1, |s| s.to_string())(input)?;
    let (input, fields) = separated_list1(char('|'), hl7_field)(input)?;
    let (input, _) = line_ending(input)?;
    Ok((
        input,
        Hl7Segment {
            segment_id,
            fields,
        },
    ))
}

fn hl7_message(input: &str) -> IResult<&str, Hl7Message> {
    let (input, segments) = separated_list1(line_ending, hl7_segment)(input)?;
    Ok((input, Hl7Message { segments }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        std::process::exit(1);
    }

    let file_path = Path::new(&args[1]);
    let file_content = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            std::process::exit(1);
        }
    };

    match hl7_message(&file_content) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(err) => {
            eprintln!("Error parsing HL7 message: {:?}", err);
            std::process::exit(1);
        }
    }
}

