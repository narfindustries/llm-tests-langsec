use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{alphanumeric1, char, line_ending},
    combinator::{map_res, opt},
    multi::separated_list0,
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

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
    let (input, (_, field_separator, encoding_characters, sending_application, sending_facility, receiving_application, receiving_facility, date_time_of_message, security, message_type, message_control_id, processing_id, version_id, _)) =
        tuple((
            tag("MSH"),
            char('|'),
            take_until("|"),
            delimited(char('|'), alphanumeric1, char('|')),
            delimited(char('|'), alphanumeric1, char('|')),
            delimited(char('|'), alphanumeric1, char('|')),
            delimited(char('|'), alphanumeric1, char('|')),
            delimited(char('|'), alphanumeric1, char('|')),
            opt(delimited(char('|'), alphanumeric1, char('|'))),
            delimited(char('|'), alphanumeric1, char('|')),
            delimited(char('|'), alphanumeric1, char('|')),
            delimited(char('|'), alphanumeric1, char('|')),
            delimited(char('|'), alphanumeric1, line_ending),
        ))(input)?;

    Ok((
        input,
        Segment::MSH(MSHSegment {
            field_separator,
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
    let (input, (_, set_id, patient_id, patient_identifier_list)) = tuple((
        tag("PID"),
        opt(preceded(char('|'), alphanumeric1)),
        opt(preceded(char('|'), alphanumeric1)),
        separated_list0(char('~'), preceded(char('|'), alphanumeric1)),
    ))(input)?;

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
    let (input, segment) = preceded(
        take_until("|"),
        alt((
            parse_msh_segment,
            parse_pid_segment,
            // Add other segment parsers here
        )),
    )(input)?;

    Ok((input, segment))
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
    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match parse_hl7_message(&contents) {
        Ok((_, message)) => {
            println!("{:#?}", message);
        }
        Err(e) => {
            println!("Failed to parse HL7 message: {:?}", e);
        }
    }

    Ok(())
}