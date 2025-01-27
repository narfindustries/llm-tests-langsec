use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{alpha1, alphanumeric1, char, digit1, line_ending, space0, space1},
    combinator::{map, map_res, opt, recognize, value},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
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
    // Add other HL7 data types as needed
}

fn hl7_string(input: &str) -> IResult<&str, Hl7DataType> {
    map(take_while1(|c: char| c.is_alphanumeric() || c == '.' || c == '-'), |s: &str| Hl7DataType::String(s.to_string()))(input)
}


fn hl7_number(input: &str) -> IResult<&str, Hl7DataType> {
    map_res(digit1, |s: &str| s.parse::<i32>(), |s| Hl7DataType::Number(s))(input)
}

fn hl7_date(input: &str) -> IResult<&str, Hl7DataType> {
    map(recognize(pair(digit1, pair(char('-'), digit1))), |s: &str| Hl7DataType::Date(s.to_string()))(input)

}

fn hl7_field(input: &str) -> IResult<&str, Hl7DataType> {
    alt((hl7_string, hl7_number, hl7_date))(input)
}

fn hl7_segment(input: &str) -> IResult<&str, Vec<Hl7DataType>> {
    let (input, fields) = many1(terminated(hl7_field, char('|')))(input)?;
    Ok((input, fields))
}

fn hl7_message(input: &str) -> IResult<&str, Vec<Vec<Hl7DataType>>> {
    let (input, segments) = many1(terminated(hl7_segment, line_ending))(input)?;
    Ok((input, segments))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <hl7_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let contents = fs::read_to_string(path).expect("Something went wrong reading the file");
    match hl7_message(&contents) {
        Ok((_, message)) => {
            println!("Parsed HL7 Message: {:?}", message);
        }
        Err(e) => {
            println!("Error parsing HL7 message: {:?}", e);
        }
    }
}
