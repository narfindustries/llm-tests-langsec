use nom::{
    bytes::complete::take_while1,
    character::complete::{alphanumeric1, char, line_ending},
    combinator::{map, recognize},
    multi::separated_list0,
    IResult,
};
use std::env;
use std::fs;
use std::path::Path;

#[derive(Debug, PartialEq)]
enum Hl7DataType {
    String(String),
    Number(i32),
    Date(String),
    // Add more data types as needed based on HL7 spec
}

fn parse_hl7_field(input: &str) -> IResult<&str, Hl7DataType> {
    let parser = recognize(alphanumeric1);
    map(parser, |s: &str| Hl7DataType::String(s.to_string()))(input)
}

fn parse_hl7_segment(input: &str) -> IResult<&str, Vec<Hl7DataType>> {
    let mut parser = separated_list0(char('|'), parse_hl7_field);
    parser(input)
}

fn parse_hl7_message(input: &str) -> IResult<&str, Vec<Vec<Hl7DataType>>> {
    let mut parser = separated_list0(line_ending, parse_hl7_segment);
    parser(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        return;
    }

    let file_path = Path::new(&args[1]);
    let file_content = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    };

    match parse_hl7_message(&file_content) {
        Ok((_, message)) => {
            println!("Parsed HL7 message:");
            for segment in message {
                println!("Segment: {:?}", segment);
            }
        }
        Err(err) => {
            eprintln!("Error parsing HL7 message: {:?}", err);
        }
    }
}
