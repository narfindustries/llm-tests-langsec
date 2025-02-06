use std::env;
use std::fs::File;
use std::io::{Read, BufReader};
use nom::{
    bytes::complete::{tag, take_while_m_n, take_while},
    character::complete::{char, digit1, multispace1},
    combinator::{map, opt, recognize},
    multi::{many0, many1},
    IResult,
};
use nom::branch::alt;
use nom::sequence::{delimited, preceded, tuple};

#[derive(Debug)]
enum DataType {
    ST(String),
    NM(f64),
    TX(String),
}

#[derive(Debug)]
enum FieldType {
    ID(String),
    IS(f64),
    NM(f64),
    ST(String),
    TX(String),
}

#[derive(Debug)]
enum SegmentType {
    MSH(MshSegment),
    EVN(EvnSegment),
    PID(PidSegment),
    PV1(Pv1Segment),
    ORC(OrSegment),
    OBR(ObrSegment),
    OBX(ObxSegment),
    DG1(Dg1Segment),
    IN1(In1Segment),
    GT1(Gt1Segment),
}

#[derive(Debug)]
struct MshSegment {
    field_separator: char,
    encoding_characters: String,
    sending_application: String,
    sending_facility: String,
    receiving_application: String,
    receiving_facility: String,
    date: String,
    time: String,
    security: Option<String>,
    message_type: MessageType,
    message_control_id: String,
    processing_id: String,
    version_id: String,
    sequence_number: Option<String>,
    continuation_pointer: Option<String>,
    accept_acknowledgment_type: Option<String>,
}

#[derive(Debug)]
struct EvnSegment {
    event_type_code: String,
    recorded_date: String,
    recorded_time: String,
    event_facility: Option<String>,
    event_staff_id: Option<String>,
    event_id: Option<String>,
}

#[derive(Debug)]
struct PidSegment {
    set_id_patient_id: String,
    patient_id_external_id: String,
    patient_id_internal_id: Option<String>,
    alternate_patient_id: Option<String>,
    patient_name: String,
    mother_s_maiden_name: Option<String>,
}

#[derive(Debug)]
struct Pv1Segment {
    set_id_patient_visit: String,
    visit_number: String,
    patient_class: String,
    visit_reason: Option<String>,
}

#[derive(Debug)]
struct OrSegment {
    order_control: String,
    placer_order_number: String,
    filler_order_number: String,
}

#[derive(Debug)]
struct ObrSegment {
    set_id_observation_request: String,
    placer_order_number: String,
    filler_order_number: String,
    universal_service_id: String,
}

#[derive(Debug)]
struct ObxSegment {
    set_id_observation_result: String,
    value_type: String,
    observation_identifier: String,
    observation_value: DataType,
}

#[derive(Debug)]
struct Dg1Segment {
    set_id_diagnosis: String,
    diagnosis_code: String,
    diagnosis_description: Option<String>,
}

#[derive(Debug)]
struct In1Segment {
    set_id_insurance: String,
    insurance_plan_id: String,
    insurance_company_id: Option<String>,
}

#[derive(Debug)]
struct Gt1Segment {
    set_id_guarantor: String,
    guarantor_number: String,
    guarantor_name: String,
}

#[derive(Debug)]
struct MessageType {
    message_type: String,
    trigger_event: String,
    message_structure: String,
}

fn field_separator(input: &str) -> IResult<&str, char> {
    char('|')(input)
}

fn encoding_characters(input: &str) -> IResult<&str, String> {
    recognize(take_while_m_n(1, 3, |c: char| c == '^' || c == '~' || c == '&'))(input)
}

fn data_type(input: &str) -> IResult<&str, DataType> {
    alt((map(recognize(take_while(|c: char| c.is_numeric() || c == '.')), |s: &str| DataType::NM(s.parse().unwrap())), 
         map(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')), |s: &str| DataType::ST(s.to_string())),
         map(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ' || c == '.')), |s: &str| DataType::TX(s.to_string()))))(input)
}

fn field_type(input: &str) -> IResult<&str, FieldType> {
    alt((map(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')), |s: &str| FieldType::ID(s.to_string())),
         map(recognize(digit1), |s: &str| FieldType::IS(s.parse().unwrap())),
         map(recognize(take_while(|c: char| c.is_numeric() || c == '.')), |s: &str| FieldType::NM(s.parse().unwrap())),
         map(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')), |s: &str| FieldType::ST(s.to_string())),
         map(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ' || c == '.')), |s: &str| FieldType::TX(s.to_string()))))(input)
}

fn message_type(input: &str) -> IResult<&str, MessageType> {
    let (input, message_type) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = char('^')(input)?;
    let (input, trigger_event) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = char('^')(input)?;
    let (input, message_structure) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    Ok((input, MessageType { message_type: message_type.to_string(), trigger_event: trigger_event.to_string(), message_structure: message_structure.to_string() }))
}

fn msh_segment(input: &str) -> IResult<&str, MshSegment> {
    let (input, _) = tag("MSH")(input)?;
    let (input, _) = field_separator(input)?;
    let (input, field_separator) = field_separator(input)?;
    let (input, encoding_characters) = encoding_characters(input)?;
    let (input, _) = field_separator(input)?;
    let (input, sending_application) = recognize(take_while(|c: char| c.is_alphabetic() || c == ' '))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, sending_facility) = recognize(take_while(|c: char| c.is_alphabetic() || c == ' '))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, receiving_application) = recognize(take_while(|c: char| c.is_alphabetic() || c == ' '))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, receiving_facility) = recognize(take_while(|c: char| c.is_alphabetic() || c == ' '))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, date) = recognize(take_while(|c: char| c.is_numeric() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, time) = recognize(take_while(|c: char| c.is_numeric() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, security) = opt(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, message_type) = message_type(input)?;
    let (input, _) = field_separator(input)?;
    let (input, message_control_id) = recognize(take_while(|c: char| c.is_alphabetic() || c == ' '))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, processing_id) = recognize(take_while(|c: char| c.is_alphabetic() || c == ' '))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, version_id) = recognize(take_while(|c: char| c.is_alphabetic() || c == ' '))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, sequence_number) = opt(recognize(take_while(|c: char| c.is_numeric() || c == '^')))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, continuation_pointer) = opt(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, accept_acknowledgment_type) = opt(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')))(input)?;
    Ok((input, MshSegment { field_separator, encoding_characters: encoding_characters.to_string(), sending_application: sending_application.to_string(), sending_facility: sending_facility.to_string(), receiving_application: receiving_application.to_string(), receiving_facility: receiving_facility.to_string(), date: date.to_string(), time: time.to_string(), security, message_type, message_control_id: message_control_id.to_string(), processing_id: processing_id.to_string(), version_id: version_id.to_string(), sequence_number, continuation_pointer, accept_acknowledgment_type }))
}

fn evn_segment(input: &str) -> IResult<&str, EvnSegment> {
    let (input, _) = tag("EVN")(input)?;
    let (input, _) = field_separator(input)?;
    let (input, event_type_code) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, recorded_date) = recognize(take_while(|c: char| c.is_numeric() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, recorded_time) = recognize(take_while(|c: char| c.is_numeric() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, event_facility) = opt(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, event_staff_id) = opt(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, event_id) = opt(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')))(input)?;
    Ok((input, EvnSegment { event_type_code: event_type_code.to_string(), recorded_date: recorded_date.to_string(), recorded_time: recorded_time.to_string(), event_facility, event_staff_id, event_id }))
}

fn pid_segment(input: &str) -> IResult<&str, PidSegment> {
    let (input, _) = tag("PID")(input)?;
    let (input, _) = field_separator(input)?;
    let (input, set_id_patient_id) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, patient_id_external_id) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, patient_id_internal_id) = opt(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, alternate_patient_id) = opt(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, patient_name) = recognize(take_while(|c: char| c.is_alphabetic() || c == ' '))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, mother_s_maiden_name) = opt(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')))(input)?;
    Ok((input, PidSegment { set_id_patient_id: set_id_patient_id.to_string(), patient_id_external_id: patient_id_external_id.to_string(), patient_id_internal_id, alternate_patient_id, patient_name: patient_name.to_string(), mother_s_maiden_name }))
}

fn pv1_segment(input: &str) -> IResult<&str, Pv1Segment> {
    let (input, _) = tag("PV1")(input)?;
    let (input, _) = field_separator(input)?;
    let (input, set_id_patient_visit) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, visit_number) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, patient_class) = recognize(take_while(|c: char| c.is_alphabetic() || c == ' '))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, visit_reason) = opt(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')))(input)?;
    Ok((input, Pv1Segment { set_id_patient_visit: set_id_patient_visit.to_string(), visit_number: visit_number.to_string(), patient_class: patient_class.to_string(), visit_reason }))
}

fn or_segment(input: &str) -> IResult<&str, OrSegment> {
    let (input, _) = tag("ORC")(input)?;
    let (input, _) = field_separator(input)?;
    let (input, order_control) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, placer_order_number) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, filler_order_number) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    Ok((input, OrSegment { order_control: order_control.to_string(), placer_order_number: placer_order_number.to_string(), filler_order_number: filler_order_number.to_string() }))
}

fn obr_segment(input: &str) -> IResult<&str, ObrSegment> {
    let (input, _) = tag("OBR")(input)?;
    let (input, _) = field_separator(input)?;
    let (input, set_id_observation_request) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, placer_order_number) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, filler_order_number) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, universal_service_id) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    Ok((input, ObrSegment { set_id_observation_request: set_id_observation_request.to_string(), placer_order_number: placer_order_number.to_string(), filler_order_number: filler_order_number.to_string(), universal_service_id: universal_service_id.to_string() }))
}

fn obx_segment(input: &str) -> IResult<&str, ObxSegment> {
    let (input, _) = tag("OBX")(input)?;
    let (input, _) = field_separator(input)?;
    let (input, set_id_observation_result) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, value_type) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, observation_identifier) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, observation_value) = data_type(input)?;
    Ok((input, ObxSegment { set_id_observation_result: set_id_observation_result.to_string(), value_type: value_type.to_string(), observation_identifier: observation_identifier.to_string(), observation_value }))
}

fn dg1_segment(input: &str) -> IResult<&str, Dg1Segment> {
    let (input, _) = tag("DG1")(input)?;
    let (input, _) = field_separator(input)?;
    let (input, set_id_diagnosis) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, diagnosis_code) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, diagnosis_description) = opt(recognize(take_while(|c: char| c.is_alphabetic() || c == ' ')))(input)?;
    Ok((input, Dg1Segment { set_id_diagnosis: set_id_diagnosis.to_string(), diagnosis_code: diagnosis_code.to_string(), diagnosis_description }))
}

fn in1_segment(input: &str) -> IResult<&str, In1Segment> {
    let (input, _) = tag("IN1")(input)?;
    let (input, _) = field_separator(input)?;
    let (input, set_id_insurance) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) = field_separator(input)?;
    let (input, insurance_plan_id) = recognize(take_while(|c: char| c.is_alphabetic() || c == '^'))(input)?;
    let (input, _) =