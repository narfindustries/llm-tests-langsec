use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{alphanumeric1, char, digit1, multispace0},
    combinator::{map_res, opt},
    multi::separated_list0,
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

fn parse_field(input: &str) -> IResult<&str, &str> {
    take_until("|")(input)
}

fn parse_component(input: &str) -> IResult<&str, &str> {
    take_until("^")(input)
}

fn parse_subcomponent(input: &str) -> IResult<&str, &str> {
    take_until("&")(input)
}

fn parse_segment(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list0(char('|'), parse_field)(input)
}

fn parse_message(input: &str) -> IResult<&str, Vec<Vec<&str>>> {
    separated_list0(char('\r'), parse_segment)(input)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match parse_message(&contents) {
        Ok((_, message)) => {
            for segment in message {
                println!("{:?}", segment);
            }
        }
        Err(e) => {
            eprintln!("Failed to parse HL7 message: {:?}", e);
        }
    }

    Ok(())
}