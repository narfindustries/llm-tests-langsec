use nom::{
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, alphanumeric1, char, digit1, line_ending},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct Hl7Message {
    segments: Vec<Hl7Segment>,
}

#[derive(Debug)]
struct Hl7Segment {
    segment_id: String,
    fields: Vec<Hl7Field>,
}

#[derive(Debug)]
struct Hl7Field {
    value: String,
}


fn hl7_field(input: &str) -> IResult<&str, Hl7Field> {
    map(
        take_while(|c: char| c != '\r' && c != '\n' && c != '|'),
        |value: &str| Hl7Field {
            value: value.to_string(),
        },
    )(input)
}

fn hl7_segment(input: &str) -> IResult<&str, Hl7Segment> {
    let (input, segment_id) = terminated(alpha1, char('|'))(input)?;
    let (input, fields) = separated_list0(char('|'), hl7_field)(input)?;
    let (input, _) = opt(line_ending)(input)?; // Handle optional line ending

    Ok((
        input,
        Hl7Segment {
            segment_id: segment_id.to_string(),
            fields,
        },
    ))
}


fn hl7_message(input: &str) -> IResult<&str, Hl7Message> {
    let (input, segments) = many0(hl7_segment)(input)?;
    Ok((input, Hl7Message { segments }))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("Failed to read file");

    match hl7_message(&contents) {
        Ok((_, message)) => {
            println!("{:#?}", message);
        }
        Err(e) => {
            eprintln!("Error parsing HL7 message: {:?}", e);
            std::process::exit(1);
        }
    }
}
