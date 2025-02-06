extern crate nom;

use nom::{
    bytes::complete::{tag, take_until},
    character::complete::newline,
    combinator::opt,
    multi::separated_list0,
    sequence::{delimited, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct Message {
    header: Vec<String>,
    segments: Vec<Segment>,
}

#[derive(Debug)]
struct Segment {
    id: String,
    fields: Vec<Option<String>>,
}

fn parse_field(input: &str) -> IResult<&str, Option<String>> {
    let (input, field) = opt(take_until("|"))(input)?;
    let (input, _) = opt(tag("|"))(input)?;
    Ok((input, field.map(|s| s.to_string())))
}

fn parse_segment(input: &str) -> IResult<&str, Segment> {
    let (input, id) = take_until("|")(input)?;
    let (input, _) = tag("|")(input)?;
    let (input, fields) = separated_list0(tag("|"), parse_field)(input)?;
    let (input, _) = opt(newline)(input)?;
    Ok((input, Segment {
        id: id.to_string(),
        fields,
    }))
}

fn parse_hl7(input: &str) -> IResult<&str, Message> {
    let (input, header) = separated_list0(tag("|"), parse_field)(input)?;
    let (input, _) = tag("\r")(input)?; // segments end with a carriage return
    let (input, segments) = separated_list0(newline, parse_segment)(input)?;
    Ok((input, Message {
        header: header.into_iter().filter_map(|x| x).collect(),
        segments,
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;

    match parse_hl7(&buffer) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse HL7 message: {:?}", e),
    }

    Ok(())
}