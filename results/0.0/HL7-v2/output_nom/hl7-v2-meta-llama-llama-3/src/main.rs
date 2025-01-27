use nom::{
    bytes::complete::{tag, take_till, take_while},
    character::complete::{char, digit1, multispace0, multispace1},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, preceded, separated_pair, tuple},
    IResult,
};
use std::{
    env,
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
    Boolean,
    Binary,
}

#[derive(Debug, PartialEq)]
enum SegmentType {
    MSH,
    EVN,
    PID,
    PV1,
    // Add more segment types as needed
}

#[derive(Debug, PartialEq)]
struct Field {
    data_type: DataType,
    value: String,
}

#[derive(Debug, PartialEq)]
struct Segment {
    segment_type: SegmentType,
    fields: Vec<Field>,
}

fn parse_field(input: &str) -> IResult<&str, Field> {
    let (input, data_type) = parse_data_type(input)?;
    let (input, _) = char('|')(input)?;
    let (input, value) = take_till(|c| c == '|')(input)?;
    Ok((input, Field { data_type, value: value.to_string() }))
}

fn parse_data_type(input: &str) -> IResult<&str, DataType> {
    let (input, first_char) = take_while(|c| c.is_alphabetic())(input)?;
    match first_char {
        "ST" => Ok((input, DataType::String)),
        "NM" => Ok((input, DataType::Numeric)),
        "DT" => Ok((input, DataType::Date)),
        "TM" => Ok((input, DataType::Time)),
        "TS" => Ok((input, DataType::DateTime)),
        "BL" => Ok((input, DataType::Boolean)),
        "BIN" => Ok((input, DataType::Binary)),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    }
}

fn parse_segment(input: &str) -> IResult<&str, Segment> {
    let (input, segment_type) = parse_segment_type(input)?;
    let (input, _) = char('|')(input)?;
    let (input, fields) = separated_list1(char('|'), parse_field)(input)?;
    Ok((input, Segment { segment_type, fields }))
}

fn parse_segment_type(input: &str) -> IResult<&str, SegmentType> {
    let (input, segment_type) = take_while(|c| c.is_alphabetic())(input)?;
    match segment_type {
        "MSH" => Ok((input, SegmentType::MSH)),
        "EVN" => Ok((input, SegmentType::EVN)),
        "PID" => Ok((input, SegmentType::PID)),
        "PV1" => Ok((input, SegmentType::PV1)),
        // Add more segment types as needed
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    }
}

fn parse_hl7(input: &str) -> IResult<&str, Vec<Segment>> {
    separated_list1(char('\n'), parse_segment)(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let file = File::open(input_file).expect("Failed to open file");
    let reader = BufReader::new(file);
    let mut input = String::new();
    for line in reader.lines() {
        input.push_str(&line.unwrap());
        input.push('\n');
    }
    let result = parse_hl7(&input);
    match result {
        Ok((_, segments)) => {
            for segment in segments {
                println!("{:?}", segment);
            }
        }
        Err(err) => {
            println!("Error parsing HL7: {:?}", err);
        }
    }
}