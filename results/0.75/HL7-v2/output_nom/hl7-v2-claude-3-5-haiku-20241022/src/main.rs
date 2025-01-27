use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::complete::{char, digit1, multispace0, not_line_ending},
    combinator::{map, opt},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, separated_pair, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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
    value: Vec<String>,
    repetitions: Vec<Vec<String>>,
    components: Vec<Vec<String>>,
}

fn parse_hl7_message(input: &[u8]) -> IResult<&[u8], HL7Message> {
    let (input, segments) = many1(parse_segment)(input)?;
    Ok((input, HL7Message { segments }))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    let (input, name) = take_while(|c| c.is_ascii_alphabetic())(input)?;
    let name = String::from_utf8_lossy(name).into_owned();
    let (input, _) = char('|')(input)?;
    let (input, fields) = separated_list0(char('|'), parse_field)(input)?;
    let (input, _) = tag(b"\r\n")(input)?;
    Ok((input, Segment { name, fields }))
}

fn parse_field(input: &[u8]) -> IResult<&[u8], Field> {
    let (input, value) = separated_list0(char('^'), parse_component)(input)?;
    let repetitions = many0(preceded_by_char_and_parse('~', parse_field))(input)?;
    Ok((
        input,
        Field {
            value: value.iter().map(|v| v.join("^")).collect(),
            repetitions: repetitions.0.iter().map(|r| r.iter().map(|v| v.join("^")).collect()).collect(),
            components: value,
        }
    ))
}

fn parse_component(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let (input, value) = take_while(|c| c != b'|' && c != b'^' && c != b'~' && c != b'\r' && c != b'\n')(input)?;
    Ok((input, vec![String::from_utf8_lossy(value).into_owned()]))
}

fn preceded_by_char_and_parse<'a, F, O>(c: char, f: F) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], O>
where
    F: Fn(&'a [u8]) -> IResult<&'a [u8], O>,
{
    move |input| {
        let (input, _) = char(c)(input)?;
        f(input)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_hl7_message(&buffer) {
        Ok((_, message)) => {
            println!("Parsed HL7 Message: {:?}", message);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}