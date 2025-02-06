use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{char, none_of},
    combinator::{map, opt},
    multi::{many0, many1},
    sequence::{delimited, tuple},
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct HL7Message {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    MSH(MSHSegment),
    PID(PIDSegment),
    OBR(OBRSegment),
    OBX(OBXSegment),
    // Add other segments
}

#[derive(Debug)]
struct MSHSegment {
    field_separator: char,
    encoding_characters: String,
    sending_application: String,
    sending_facility: String,
    receiving_application: String,
    receiving_facility: String,
    datetime: String,
    security: Option<String>,
    message_type: String,
    message_control_id: String,
    processing_id: String,
    version_id: String,
}

#[derive(Debug)]
struct PIDSegment {
    set_id: String,
    patient_id: String,
    patient_identifier_list: Vec<String>,
    patient_name: Vec<String>,
    mother_maiden_name: Option<String>,
    birth_datetime: Option<String>,
    admin_sex: Option<String>,
    patient_alias: Option<Vec<String>>,
    race: Option<Vec<String>>,
    patient_address: Option<Vec<String>>,
    country_code: Option<String>,
    phone_number_home: Option<Vec<String>>,
    phone_number_business: Option<Vec<String>>,
}

#[derive(Debug)]
struct OBRSegment {
    set_id: String,
    placer_order_number: Option<String>,
    filler_order_number: Option<String>,
    universal_service_id: String,
    priority: Option<String>,
    requested_datetime: Option<String>,
    observation_datetime: Option<String>,
    observation_end_datetime: Option<String>,
    collection_volume: Option<String>,
    collector_identifier: Option<Vec<String>>,
    specimen_action_code: Option<String>,
    danger_code: Option<Vec<String>>,
}

#[derive(Debug)]
struct OBXSegment {
    set_id: String,
    value_type: String,
    observation_identifier: String,
    observation_sub_id: Option<String>,
    observation_value: Vec<String>,
    units: Option<String>,
    references_range: Option<String>,
    abnormal_flags: Option<Vec<String>>,
    probability: Option<String>,
    observation_result_status: String,
}

fn parse_field(input: &str) -> IResult<&str, String> {
    map(
        many0(none_of("|^~\\&")),
        |chars: Vec<char>| chars.into_iter().collect(),
    )(input)
}

fn parse_component(input: &str) -> IResult<&str, String> {
    map(
        many0(none_of("^|~\\&")),
        |chars: Vec<char>| chars.into_iter().collect(),
    )(input)
}

fn parse_msh_segment(input: &str) -> IResult<&str, MSHSegment> {
    let (input, _) = tag("MSH")(input)?;
    let (input, field_separator) = char('|')(input)?;
    let (input, encoding_characters) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, sending_application) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, sending_facility) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, receiving_application) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, receiving_facility) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, datetime) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, security) = opt(parse_field)(input)?;
    let (input, _) = char('|')(input)?;
    let (input, message_type) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, message_control_id) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, processing_id) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, version_id) = parse_field(input)?;

    Ok((
        input,
        MSHSegment {
            field_separator,
            encoding_characters,
            sending_application,
            sending_facility,
            receiving_application,
            receiving_facility,
            datetime,
            security,
            message_type,
            message_control_id,
            processing_id,
            version_id,
        },
    ))
}

fn parse_pid_segment(input: &str) -> IResult<&str, PIDSegment> {
    let (input, _) = tag("PID|")(input)?;
    let (input, set_id) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_id) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_identifier_list) = many1(delimited(char('~'), parse_component, opt(char('~'))))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_name) = many1(delimited(char('^'), parse_component, opt(char('^'))))(input)?;
    // Continue parsing other PID fields...

    Ok((
        input,
        PIDSegment {
            set_id,
            patient_id,
            patient_identifier_list,
            patient_name,
            mother_maiden_name: None,
            birth_datetime: None,
            admin_sex: None,
            patient_alias: None,
            race: None,
            patient_address: None,
            country_code: None,
            phone_number_home: None,
            phone_number_business: None,
        },
    ))
}

fn parse_hl7_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, segments) = many1(map(
        take_until("\r"),
        |segment: &str| match &segment[0..3] {
            "MSH" => Segment::MSH(parse_msh_segment(segment).unwrap().1),
            "PID" => Segment::PID(parse_pid_segment(segment).unwrap().1),
            // Add other segment parsers
            _ => panic!("Unknown segment type"),
        },
    ))(input)?;

    Ok((input, HL7Message { segments }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input = fs::read_to_string(&args[1]).expect("Failed to read input file");
    match parse_hl7_message(&input) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse HL7 message: {:?}", e),
    }
}