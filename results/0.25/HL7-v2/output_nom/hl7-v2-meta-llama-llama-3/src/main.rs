use nom::{
    bits::complete::tag,
    bytes::complete::{is_not, take_till, take_while_m_n},
    character::complete::{char, digit1, newline, space0, space1},
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
use std::convert::TryFrom;

#[derive(Debug)]
enum Hl7SegmentId {
    MSH,
    EVN,
    PID,
    PV1,
    OBR,
    OBX,
    Other(String),
}

impl TryFrom<&str> for Hl7SegmentId {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s.to_uppercase().as_str() {
            "MSH" => Ok(Hl7SegmentId::MSH),
            "EVN" => Ok(Hl7SegmentId::EVN),
            "PID" => Ok(Hl7SegmentId::PID),
            "PV1" => Ok(Hl7SegmentId::PV1),
            "OBR" => Ok(Hl7SegmentId::OBR),
            "OBX" => Ok(Hl7SegmentId::OBX),
            _ => Ok(Hl7SegmentId::Other(s.to_string())),
        }
    }
}

#[derive(Debug)]
struct Hl7Segment {
    id: Hl7SegmentId,
    fields: Vec<String>,
}

fn hl7_segment_id(input: &str) -> IResult<&str, Hl7SegmentId> {
    map_res(recognize(is_not("|")), |s: &str| s.try_from(s))(
        input,
    )
}

fn hl7_field(input: &str) -> IResult<&str, String> {
    recognize(take_till(|c| c == '|' || c == '\r' || c == '\n'))(input)
}

fn hl7_segment(input: &str) -> IResult<&str, Hl7Segment> {
    let (input, id) = hl7_segment_id(input)?;
    let (input, _) = char('|')(input)?;
    let (input, fields) = many1(hl7_field)(input)?;
    let fields = fields
        .into_iter()
        .filter(|f| f != "")
        .collect::<Vec<_>>();
    Ok((input, Hl7Segment { id, fields }))
}

fn hl7_message(input: &str) -> IResult<&str, Vec<Hl7Segment>> {
    many1(hl7_segment)(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let file = File::open(filename).expect("Unable to open file!");
    let reader = BufReader::new(file);
    let mut content = String::new();
    for line in reader.lines() {
        content.push_str(&line.unwrap());
        content.push('\n');
    }
    let (_remaining, message) = hl7_message(&content).unwrap();
    println!("{:?}", message);
}