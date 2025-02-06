use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{char, none_of},
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, tuple},
    IResult,
};
use std::{env, fs};

#[derive(Debug)]
struct HL7Message {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    MSH(MSHSegment),
    PID(PIDSegment),
    // Add other segments
}

#[derive(Debug)]
struct MSHSegment {
    field_separator: char,
    encoding_chars: String,
    sending_app: Option<String>,
    sending_facility: Option<String>,
    receiving_app: Option<String>,
    receiving_facility: Option<String>,
    datetime: Option<String>,
    security: Option<String>,
    message_type: Option<String>,
    message_control_id: Option<String>,
    processing_id: Option<String>,
    version_id: Option<String>,
}

#[derive(Debug)]
struct PIDSegment {
    set_id: Option<String>,
    patient_id: Option<String>,
    patient_id_list: Option<String>,
    alternate_id: Option<String>,
    patient_name: Option<String>,
    mother_maiden_name: Option<String>,
    birth_date: Option<String>,
    sex: Option<String>,
}

fn parse_field(input: &str) -> IResult<&str, Option<String>> {
    let (input, field) = opt(take_until("|"))(input)?;
    Ok((input, field.map(|s| s.to_string())))
}

fn parse_msh_segment(input: &str) -> IResult<&str, MSHSegment> {
    let (input, _) = tag("MSH")(input)?;
    let (input, field_separator) = char('|')(input)?;
    let (input, encoding_chars) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, (
        sending_app,
        sending_facility,
        receiving_app,
        receiving_facility,
        datetime,
        security,
        message_type,
        message_control_id,
        processing_id,
        version_id,
    )) = tuple((
        parse_field,
        parse_field,
        parse_field,
        parse_field,
        parse_field,
        parse_field,
        parse_field,
        parse_field,
        parse_field,
        parse_field,
    ))(input)?;

    Ok((input, MSHSegment {
        field_separator,
        encoding_chars: encoding_chars.to_string(),
        sending_app,
        sending_facility,
        receiving_app,
        receiving_facility,
        datetime,
        security,
        message_type,
        message_control_id,
        processing_id,
        version_id,
    }))
}

fn parse_pid_segment(input: &str) -> IResult<&str, PIDSegment> {
    let (input, _) = tag("PID|")(input)?;
    let (input, (
        set_id,
        patient_id,
        patient_id_list,
        alternate_id,
        patient_name,
        mother_maiden_name,
        birth_date,
        sex,
    )) = tuple((
        parse_field,
        parse_field,
        parse_field,
        parse_field,
        parse_field,
        parse_field,
        parse_field,
        parse_field,
    ))(input)?;

    Ok((input, PIDSegment {
        set_id,
        patient_id,
        patient_id_list,
        alternate_id,
        patient_name,
        mother_maiden_name,
        birth_date,
        sex,
    }))
}

fn parse_segment(input: &str) -> IResult<&str, Segment> {
    let (input, segment_type) = take_until("|")(input)?;
    match segment_type {
        "MSH" => map(parse_msh_segment, Segment::MSH)(input),
        "PID" => map(parse_pid_segment, Segment::PID)(input),
        _ => panic!("Unsupported segment type: {}", segment_type),
    }
}

fn parse_hl7_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, segments) = many0(parse_segment)(input)?;
    Ok((input, HL7Message { segments }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        std::process::exit(1);
    }

    let content = fs::read_to_string(&args[1])?;
    match parse_hl7_message(&content) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse HL7 message: {}", e),
    }

    Ok(())
}