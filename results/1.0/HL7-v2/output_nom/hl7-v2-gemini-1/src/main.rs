use nom::{
    bytes::complete::{tag, take_while},
    character::complete::{alphanumeric1, char, line_ending},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult,
};
use std::env;
use std::fs;
use std::io::{self, Read};

#[derive(Debug, PartialEq, Eq)]
struct Hl7Message {
    segments: Vec<Hl7Segment>,
}

#[derive(Debug, PartialEq, Eq)]
struct Hl7Segment {
    field_separator: char,
    fields: Vec<Hl7Field>,
}

#[derive(Debug, PartialEq, Eq)]
struct Hl7Field {
    components: Vec<Hl7Component>,
}

#[derive(Debug, PartialEq, Eq)]
struct Hl7Component {
    subcomponents: Vec<String>,
}

fn hl7_component(input: &str) -> IResult<&str, Hl7Component> {
    let subcomponent_sep = tag("^");
    let subcomponent = map_res(alphanumeric1, |s| s.to_string());

    map(
        separated_list1(subcomponent_sep, subcomponent),
        |subcomponents| Hl7Component { subcomponents },
    )(input)
}

fn hl7_field(input: &str, field_sep: char) -> IResult<&str, Hl7Field> {
    let component_sep = char(field_sep);
    let component = hl7_component;

    map(
        separated_list1(component_sep, component),
        |components| Hl7Field { components },
    )(input)
}


fn hl7_segment(input: &str) -> IResult<&str, Hl7Segment> {
    let field_sep =  char('|');
    let field = |input| hl7_field(input, '|');
    let segment = tuple((field_sep, many0(field)));

    map(segment, |(field_sep, fields)| Hl7Segment {
        field_separator: field_sep,
        fields
    })(input)
}

fn hl7_message(input: &str) -> IResult<&str, Hl7Message> {
    let segment = hl7_segment;
    let segments = many0(preceded(line_ending, segment));

    map(segments, |segments| Hl7Message { segments })(input)
}


fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }
    let filename = &args[1];
    let mut file = fs::File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match hl7_message(&contents) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Error parsing HL7 message: {:?}", e),
    }

    Ok(())
}
