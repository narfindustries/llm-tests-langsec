use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{alphanumeric1, char, line_ending, multispace0},
    combinator::{map_res, opt},
    multi::separated_list0,
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct HL7Message {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    MSH(MSHSegment),
    PID(PIDSegment),
    // Add other segment types as needed
}

#[derive(Debug)]
struct MSHSegment {
    field_separator: char,
    encoding_characters: String,
    sending_application: String,
    sending_facility: String,
    receiving_application: String,
    receiving_facility: String,
    date_time_of_message: String,
    security: Option<String>,
    message_type: String,
    message_control_id: String,
    processing_id: String,
    version_id: String,
}

#[derive(Debug)]
struct PIDSegment {
    set_id: Option<String>,
    patient_id: Option<String>,
    patient_identifier_list: Vec<String>,
    // Add other fields as needed
}

fn parse_msh_segment(input: &str) -> IResult<&str, Segment> {
    let (input, _) = tag("MSH")(input)?;
    let (input, field_separator) = take_until("|")(input)?;
    let (input, encoding_characters) = delimited(char('|'), alphanumeric1, char('|'))(input)?;
    let (input, sending_application) = delimited(char('|'), alphanumeric1, char('|'))(input)?;
    let (input, sending_facility) = delimited(char('|'), alphanumeric1, char('|'))(input)?;
    let (input, receiving_application) = delimited(char('|'), alphanumeric1, char('|'))(input)?;
    let (input, receiving_facility) = delimited(char('|'), alphanumeric1, char('|'))(input)?;
    let (input, date_time_of_message) = delimited(char('|'), alphanumeric1, char('|'))(input)?;
    let (input, security) = opt(delimited(char('|'), alphanumeric1, char('|')))(input)?;
    let (input, message_type) = delimited(char('|'), alphanumeric1, char('|'))(input)?;
    let (input, message_control_id) = delimited(char('|'), alphanumeric1, char('|'))(input)?;
    let (input, processing_id) = delimited(char('|'), alphanumeric1, char('|'))(input)?;
    let (input, version_id) = delimited(char('|'), alphanumeric1, multispace0)(input)?;

    Ok((
        input,
        Segment::MSH(MSHSegment {
            field_separator: field_separator.chars().next().unwrap(),
            encoding_characters: encoding_characters.to_string(),
            sending_application: sending_application.to_string(),
            sending_facility: sending_facility.to_string(),
            receiving_application: receiving_application.to_string(),
            receiving_facility: receiving_facility.to_string(),
            date_time_of_message: date_time_of_message.to_string(),
            security: security.map(|s| s.to_string()),
            message_type: message_type.to_string(),
            message_control_id: message_control_id.to_string(),
            processing_id: processing_id.to_string(),
            version_id: version_id.to_string(),
        }),
    ))
}

fn parse_pid_segment(input: &str) -> IResult<&str, Segment> {
    let (input, _) = tag("PID")(input)?;
    let (input, set_id) = opt(delimited(char('|'), alphanumeric1, char('|')))(input)?;
    let (input, patient_id) = opt(delimited(char('|'), alphanumeric1, char('|')))(input)?;
    let (input, patient_identifier_list) = separated_list0(char('~'), delimited(char('|'), alphanumeric1, char('|')))(input)?;

    Ok((
        input,
        Segment::PID(PIDSegment {
            set_id: set_id.map(|s| s.to_string()),
            patient_id: patient_id.map(|s| s.to_string()),
            patient_identifier_list: patient_identifier_list.iter().map(|&s| s.to_string()).collect(),
        }),
    ))
}

fn parse_segments(input: &str) -> IResult<&str, Vec<Segment>> {
    separated_list0(line_ending, parse_segment)(input)
}

fn parse_segment(input: &str) -> IResult<&str, Segment> {
    preceded(multispace0, alt((parse_msh_segment, parse_pid_segment)))(input)
}

fn parse_hl7_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, segments) = parse_segments(input)?;
    Ok((input, HL7Message { segments }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No input file specified"));
    }

    let mut file = File::open(&args[1])?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match parse_hl7_message(&contents) {
        Ok((_, message)) => {
            println!("{:#?}", message);
        }
        Err(e) => {
            eprintln!("Failed to parse HL7 message: {:?}", e);
        }
    }

    Ok(())
}