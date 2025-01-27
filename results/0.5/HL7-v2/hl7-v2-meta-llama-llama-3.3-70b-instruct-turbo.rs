use nom::{
    bytes::complete::{tag, take_till, take_while_m_n},
    character::complete::{digit1, multispace0, multispace1},
    combinator::{map, map_res, opt, verify},
    error::{Error, ErrorKind},
    multi::{length_data, many0, many1, separated_list0, separated_list1},
    sequence::{delimited, preceded, separated_pair, tuple},
    IResult,
};
use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

#[derive(Debug, PartialEq)]
enum DataType {
    String,
    Numeric,
    Date,
    Time,
    DateTime,
    Cwe { identifier: String, text: String },
    Cne { identifier: String, text: String },
    Dtm,
}

#[derive(Debug, PartialEq)]
struct Component {
    data_type: DataType,
}

#[derive(Debug, PartialEq)]
struct Field {
    components: Vec<Component>,
}

#[derive(Debug, PartialEq)]
struct Segment {
    name: String,
    fields: Vec<Field>,
}

#[derive(Debug, PartialEq)]
struct Message {
    segments: Vec<Segment>,
}

fn parse_data_type(input: &str) -> IResult<&str, DataType> {
    alt((
        map(tag("ST"), |_| DataType::String),
        map(tag("NM"), |_| DataType::Numeric),
        map(tag("DT"), |_| DataType::Date),
        map(tag("TM"), |_| DataType::Time),
        map(tag("DTM"), |_| DataType::DateTime),
        map(
            tuple((tag("CWE"), take_till(|c| c == '|' || c == '\r'))),
            |(_, identifier): (&str, &str)| {
                DataType::Cwe {
                    identifier: identifier.to_string(),
                    text: "".to_string(),
                }
            },
        ),
        map(
            tuple((tag("CNE"), take_till(|c| c == '|' || c == '\r'))),
            |(_, identifier): (&str, &str)| {
                DataType::Cne {
                    identifier: identifier.to_string(),
                    text: "".to_string(),
                }
            },
        ),
        map(tag("DTM"), |_| DataType::Dtm),
    ))(input)
}

fn parse_component(input: &str) -> IResult<&str, Component> {
    map(parse_data_type, |data_type| Component { data_type })(input)
}

fn parse_field(input: &str) -> IResult<&str, Field> {
    map(
        separated_list0(tag("|"), parse_component),
        |components| Field { components },
    )(input)
}

fn parse_segment(input: &str) -> IResult<&str, Segment> {
    map(
        tuple((take_while(|c| c.is_alphabetic()), multispace1, separated_list0(tag("\r"), parse_field))),
        |(name, _, fields)| Segment { name: name.to_string(), fields },
    )(input)
}

fn parse_message(input: &str) -> IResult<&str, Message> {
    map(
        separated_list1(tag("\r"), parse_segment),
        |segments| Message { segments },
    )(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "Invalid input",
        ));
    }

    let file = File::open(&args[1])?;
    let reader = BufReader::new(file);
    let mut contents = String::new();
    for line in reader.lines() {
        contents.push_str(&line.unwrap());
        contents.push('\n');
    }

    match parse_message(&contents) {
        Ok((_, message)) => {
            println!("{:?}", message);
        }
        Err(err) => {
            eprintln!("Error parsing message: {:?}", err);
        }
    }

    Ok(())
}