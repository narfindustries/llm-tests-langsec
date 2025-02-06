use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{char, digit1, space1},
    multi::{many0, separated_list0},
    sequence::{tuple, preceded, separated_pair},
    IResult, branch::alt,
};
use std::env;
use std::fs;
use std::error::Error;

#[derive(Debug)]
struct HL7Message {
    segments: Vec<Segment>
}

#[derive(Debug)]
struct Segment {
    name: String,
    fields: Vec<Field>
}

#[derive(Debug)]
struct Field {
    components: Vec<String>
}

fn parse_field(input: &[u8]) -> IResult<&[u8], Field> {
    let (input, components) = separated_list0(
        char('^'), 
        take_until("|")
    )(input)?;

    Ok((input, Field { 
        components: components.iter().map(|&x| String::from_utf8_lossy(x).to_string()).collect() 
    }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, segment_name) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    
    let (input, fields) = separated_list0(
        char('|'), 
        parse_field
    )(input)?;

    Ok((input, Segment { 
        name: String::from_utf8_lossy(segment_name).to_string(), 
        fields 
    }))
}

fn parse_hl7_message(input: &[u8]) -> IResult<&[u8], HL7Message> {
    let (input, segments) = many0(parse_segment)(input)?;
    Ok((input, HL7Message { segments }))
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
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
            Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidData, 
                "Failed to parse HL7 message"
            )))
        }
    }
}