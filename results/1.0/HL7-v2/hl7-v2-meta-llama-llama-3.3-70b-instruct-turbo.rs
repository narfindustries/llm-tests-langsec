use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while},
    character::complete::{char, digit1, multispace0, multispace1, newline},
    combinator::{map, map_res, opt, recognize},
    multiple::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};
use std::io::prelude::*;

#[derive(Debug, PartialEq)]
enum EncodingCharacters {
    ComponentSeparator,
    FieldSeparator,
    RepetitionSeparator,
    EscapeCharacter,
    SubcomponentSeparator,
}

#[derive(Debug, PartialEq)]
enum MessageStructure {
    Segments(Vec<Segment>),
}

#[derive(Debug, PartialEq)]
enum Segment {
    MSH(MessageHeaderSegment),
    EVN(EventTypeSegment),
    PID(PatientIdentificationSegment),
    PV1(VisitSegment),
    // Add more segment types as needed
}

#[derive(Debug, PartialEq)]
struct MessageHeaderSegment {
    field_separator: char,
    encoding_characters: EncodingCharacters,
    sending_facility: String,
    sending_application: String,
    receiving_facility: String,
    receiving_application: String,
    message_date: String,
    message_time: String,
    security: String,
    message_type: MessageType,
    message_control_id: String,
    processing_id: String,
    version_id: String,
    sequence_number: String,
}

#[derive(Debug, PartialEq)]
struct EventTypeSegment {
    event_type_code: String,
    event_type_description: String,
    event_date: String,
    event_time: String,
    event_facility: String,
}

#[derive(Debug, PartialEq)]
struct PatientIdentificationSegment {
    patient_id: String,
    patient_name: String,
    date_of_birth: String,
    sex: String,
    patient_address: String,
}

#[derive(Debug, PartialEq)]
struct VisitSegment {
    visit_number: String,
    patient_class: String,
    visit_number_unique: String,
    visit_number_unique_facility: String,
}

#[derive(Debug, PartialEq)]
enum MessageType {
    ACK,
    ADT,
    BAR,
    // Add more message types as needed
}

fn encoding_characters(input: &str) -> IResult<&str, EncodingCharacters> {
    alt((
        map(tag("^"), |_| EncodingCharacters::ComponentSeparator),
        map(tag("&"), |_| EncodingCharacters::FieldSeparator),
        map(tag("~"), |_| EncodingCharacters::RepetitionSeparator),
        map(tag("\\"), |_| EncodingCharacters::EscapeCharacter),
        map(tag("|"), |_| EncodingCharacters::SubcomponentSeparator),
    ))(input)
}

fn message_header_segment(input: &str) -> IResult<&str, MessageHeaderSegment> {
    let (input, _) = tag("MSH")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, field_separator) = char(input)?;
    let (input, _) = multispace0(input)?;
    let (input, encoding_characters) = encoding_characters(input)?;
    let (input, _) = multispace0(input)?;
    let (input, sending_facility) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, sending_application) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, receiving_facility) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, receiving_application) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, message_date) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, message_time) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, security) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, message_type) = alt((
        map(tag("ACK"), |_| MessageType::ACK),
        map(tag("ADT"), |_| MessageType::ADT),
        map(tag("BAR"), |_| MessageType::BAR),
        // Add more message types as needed
    ))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, message_control_id) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, processing_id) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, version_id) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, sequence_number) = take_while(|c| c != '^')(input)?;
    Ok((
        input,
        MessageHeaderSegment {
            field_separator,
            encoding_characters,
            sending_facility: sending_facility.to_string(),
            sending_application: sending_application.to_string(),
            receiving_facility: receiving_facility.to_string(),
            receiving_application: receiving_application.to_string(),
            message_date: message_date.to_string(),
            message_time: message_time.to_string(),
            security: security.to_string(),
            message_type,
            message_control_id: message_control_id.to_string(),
            processing_id: processing_id.to_string(),
            version_id: version_id.to_string(),
            sequence_number: sequence_number.to_string(),
        },
    ))
}

fn event_type_segment(input: &str) -> IResult<&str, EventTypeSegment> {
    let (input, _) = tag("EVN")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, event_type_code) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, event_type_description) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, event_date) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, event_time) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, event_facility) = take_while(|c| c != '^')(input)?;
    Ok((
        input,
        EventTypeSegment {
            event_type_code: event_type_code.to_string(),
            event_type_description: event_type_description.to_string(),
            event_date: event_date.to_string(),
            event_time: event_time.to_string(),
            event_facility: event_facility.to_string(),
        },
    ))
}

fn patient_identification_segment(input: &str) -> IResult<&str, PatientIdentificationSegment> {
    let (input, _) = tag("PID")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, patient_id) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, patient_name) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, date_of_birth) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, sex) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, patient_address) = take_while(|c| c != '^')(input)?;
    Ok((
        input,
        PatientIdentificationSegment {
            patient_id: patient_id.to_string(),
            patient_name: patient_name.to_string(),
            date_of_birth: date_of_birth.to_string(),
            sex: sex.to_string(),
            patient_address: patient_address.to_string(),
        },
    ))
}

fn visit_segment(input: &str) -> IResult<&str, VisitSegment> {
    let (input, _) = tag("PV1")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, visit_number) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, patient_class) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, visit_number_unique) = take_while(|c| c != '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, visit_number_unique_facility) = take_while(|c| c != '^')(input)?;
    Ok((
        input,
        VisitSegment {
            visit_number: visit_number.to_string(),
            patient_class: patient_class.to_string(),
            visit_number_unique: visit_number_unique.to_string(),
            visit_number_unique_facility: visit_number_unique_facility.to_string(),
        },
    ))
}

fn segment(input: &str) -> IResult<&str, Segment> {
    alt((
        map(message_header_segment, Segment::MSH),
        map(event_type_segment, Segment::EVN),
        map(patient_identification_segment, Segment::PID),
        map(visit_segment, Segment::PV1),
        // Add more segment types as needed
    ))(input)
}

fn message_structure(input: &str) -> IResult<&str, MessageStructure> {
    let (input, segments) = many1(delimited(multispace0, segment, multispace0))(input)?;
    Ok((input, MessageStructure::Segments(segments)))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let file = File::open(filename)?;
    let reader = BufReader::new(file);
    for line in reader.lines() {
        let line = line?;
        let (_remaining, message) = message_structure(&line).unwrap();
        println!("{:?}", message);
    }
    Ok(())
}