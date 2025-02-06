use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{char, digit1},
    combinator::{opt, map_res},
    multi::{many0, separated_list0},
    sequence::{tuple, preceded},
    IResult,
    branch::alt,
    error::ParseError,
};
use std::fs::File;
use std::io::Read;
use std::env;
use std::path::Path;

#[derive(Debug)]
struct HL7Message {
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

fn parse_field(input: &str) -> IResult<&str, Field> {
    let (input, components) = separated_list0(
        char('^'),
        map_res(take_until("^|"), |s: &str| Ok::<_, ()>(s.to_string()))
    )(input)?;

    Ok((input, Field { components }))
}

fn parse_segment(input: &str) -> IResult<&str, Segment> {
    let (input, name) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    
    let (input, fields) = separated_list0(
        char('|'),
        parse_field
    )(input)?;

    Ok((input, Segment {
        name: name.to_string(),
        fields,
    }))
}

fn parse_hl7_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, segments) = many0(parse_segment)(input)?;

    Ok((input, HL7Message { segments }))
}

fn read_file(path: &Path) -> Result<String, std::io::Error> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        std::process::exit(1);
    }

    let file_path = Path::new(&args[1]);
    let contents = read_file(file_path)?;

    match parse_hl7_message(&contents) {
        Ok((_, message)) => {
            println!("Parsed HL7 Message: {:?}", message);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}