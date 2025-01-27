use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{alphanumeric1, char, line_ending},
    combinator::{map_res, opt},
    multi::separated_list0,
    sequence::{preceded, terminated, tuple},
    IResult,
};
use std::{env, fs::File, io::Read, str::from_utf8};

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
    field_separator: String,
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
    map_res(
        preceded(
            tag("MSH"),
            tuple((
                take_until("|"),
                preceded(char('|'), take_until("|")),
                preceded(char('|'), take_until("|")),
                preceded(char('|'), take_until("|")),
                preceded(char('|'), take_until("|")),
                preceded(char('|'), take_until("|")),
                preceded(char('|'), take_until("|")),
                opt(preceded(char('|'), take_until("|"))),
                preceded(char('|'), take_until("|")),
                preceded(char('|'), take_until("|")),
                preceded(char('|'), take_until("|")),
                preceded(char('|'), take_until("|")),
            )),
        ),
        |(
            field_separator,
            encoding_characters,
            sending_application,
            sending_facility,
            receiving_application,
            receiving_facility,
            date_time_of_message,
            security,
            message_type,
            message_control_id,
            processing_id,
            version_id,
        )| {
            Ok(Segment::MSH(MSHSegment {
                field_separator: field_separator.to_string(),
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
            }))
        },
    )(input)
}

fn parse_pid_segment(input: &str) -> IResult<&str, Segment> {
    map_res(
        preceded(
            tag("PID"),
            tuple((
                opt(preceded(char('|'), alphanumeric1)),
                opt(preceded(char('|'), alphanumeric1)),
                separated_list0(char('~'), preceded(char('|'), alphanumeric1)),
            )),
        ),
        |(set_id, patient_id, patient_identifier_list)| {
            Ok(Segment::PID(PIDSegment {
                set_id: set_id.map(|s| s.to_string()),
                patient_id: patient_id.map(|s| s.to_string()),
                patient_identifier_list: patient_identifier_list.iter().map(|s| s.to_string()).collect(),
            }))
        },
    )(input)
}

fn parse_segments(input: &str) -> IResult<&str, Vec<Segment>> {
    separated_list0(line_ending, parse_segment)(input)
}

fn parse_segment(input: &str) -> IResult<&str, Segment> {
    preceded(
        opt(char('\n')),
        alt((parse_msh_segment, parse_pid_segment)),
    )(input)
}

fn parse_hl7_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, segments) = parse_segments(input)?;
    Ok((input, HL7Message { segments }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut contents = Vec::new();
    file.read_to_end(&mut contents).expect("Failed to read file");
    let contents = from_utf8(&contents).expect("File content is not valid UTF-8");

    match parse_hl7_message(contents) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => println!("Error parsing HL7 message: {:?}", e),
    }
}