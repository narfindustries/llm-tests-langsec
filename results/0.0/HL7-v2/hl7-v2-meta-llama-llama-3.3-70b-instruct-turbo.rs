use nom::{
    bytes::complete::{tag, take_while_m_n},
    character::complete::{char, digit1, multispace0, multispace1},
    combinator::{map, map_res, opt, value},
    error::{Error, ErrorKind},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::{
    collections::HashMap,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

#[derive(Debug, PartialEq)]
enum EncodingCharacters {
    Default,
    Custom(String),
}

#[derive(Debug, PartialEq)]
enum FieldSeparator {
    Default,
    Custom(char),
}

#[derive(Debug, PartialEq)]
enum ComponentSeparator {
    Default,
    Custom(char),
}

#[derive(Debug, PartialEq)]
enum RepetitionSeparator {
    Default,
    Custom(char),
}

#[derive(Debug, PartialEq)]
enum EscapeCharacter {
    Default,
    Custom(char),
}

#[derive(Debug, PartialEq)]
enum SubcomponentSeparator {
    Default,
    Custom(char),
}

#[derive(Debug, PartialEq)]
enum SegmentTerminator {
    Default,
    Custom(String),
}

#[derive(Debug, PartialEq)]
enum Message {
    MSH {
        field_separator: FieldSeparator,
        encoding_characters: EncodingCharacters,
        sending_application: String,
        sending_facility: String,
        receiving_application: String,
        receiving_facility: String,
        date_time_of_message: String,
        security: String,
        message_type: String,
        message_control_id: String,
        processing_id: String,
        version_id: String,
    },
    PID {
        set_id_patient_id: String,
        patient_id: String,
        patient_id_identifier: String,
        alternate_patient_id: Option<String>,
        patient_name: String,
        mothers_maiden_name: Option<String>,
        date_of_birth: String,
        sex: String,
        patient_alias: Option<String>,
        race: Option<String>,
        patient_address: Option<String>,
    },
    PV1 {
        set_id_patient_visit: String,
        visit_number: String,
        patient_class: String,
        visit_number_identifier: Option<String>,
        financial_class: Option<String>,
        pre_admit_number: Option<String>,
        prior_patient_location: Option<String>,
        admitting_doctor: Option<String>,
        hospital_service: Option<String>,
        visit_type: Option<String>,
    },
    OBR {
        set_id_observation_request: String,
        placer_order_number: String,
        filler_order_number: Option<String>,
        universal_service_identifier: String,
        priority: Option<String>,
        requested_date_time: Option<String>,
        observation_date_time: Option<String>,
        observation_result_status: Option<String>,
        who_subject_definition: Option<String>,
    },
    OBX {
        set_id_observation_result: String,
        observation_identifier: String,
        observation_sub_id: Option<String>,
        observation_value: String,
        units: Option<String>,
        reference_range: Option<String>,
        abnormal_flags: Option<String>,
        probability: Option<String>,
        nature_of_abnormal_test: Option<String>,
        observation_result_status: Option<String>,
        date_last_observation_normal: Option<String>,
    },
}

fn encoding_characters(input: &str) -> IResult<&str, EncodingCharacters> {
    nom::branch::alt((
        value(EncodingCharacters::Default, tag("^~\\&")),
        map(take_while_m_n(1, 3, |c| c == '^' || c == '~' || c == '\\'), |s: &str| {
            EncodingCharacters::Custom(s.to_string())
        }),
    ))(input)
}

fn field_separator(input: &str) -> IResult<&str, FieldSeparator> {
    nom::branch::alt((
        value(FieldSeparator::Default, char('|')),
        map(char('|'), |_| FieldSeparator::Default),
        map(take_while_m_n(1, 1, |c| c == '|' || c == '^' || c == '&' || c == '~'), |s: &str| {
            FieldSeparator::Custom(s.chars().next().unwrap())
        }),
    ))(input)
}

fn component_separator(input: &str) -> IResult<&str, ComponentSeparator> {
    nom::branch::alt((
        value(ComponentSeparator::Default, char('^')),
        map(char('^'), |_| ComponentSeparator::Default),
        map(take_while_m_n(1, 1, |c| c == '^' || c == '&' || c == '~'), |s: &str| {
            ComponentSeparator::Custom(s.chars().next().unwrap())
        }),
    ))(input)
}

fn repetition_separator(input: &str) -> IResult<&str, RepetitionSeparator> {
    nom::branch::alt((
        value(RepetitionSeparator::Default, char('~')),
        map(char('~'), |_| RepetitionSeparator::Default),
        map(take_while_m_n(1, 1, |c| c == '~' || c == '&' || c == '^'), |s: &str| {
            RepetitionSeparator::Custom(s.chars().next().unwrap())
        }),
    ))(input)
}

fn escape_character(input: &str) -> IResult<&str, EscapeCharacter> {
    nom::branch::alt((
        value(EscapeCharacter::Default, char('\\')),
        map(char('\\'), |_| EscapeCharacter::Default),
        map(take_while_m_n(1, 1, |c| c == '\\' || c == '&' || c == '^' || c == '~'), |s: &str| {
            EscapeCharacter::Custom(s.chars().next().unwrap())
        }),
    ))(input)
}

fn subcomponent_separator(input: &str) -> IResult<&str, SubcomponentSeparator> {
    nom::branch::alt((
        value(SubcomponentSeparator::Default, char('&')),
        map(char('&'), |_| SubcomponentSeparator::Default),
        map(take_while_m_n(1, 1, |c| c == '&' || c == '^' || c == '~'), |s: &str| {
            SubcomponentSeparator::Custom(s.chars().next().unwrap())
        }),
    ))(input)
}

fn segment_terminator(input: &str) -> IResult<&str, SegmentTerminator> {
    nom::branch::alt((
        value(SegmentTerminator::Default, tag("\r")),
        map(tag("\r"), |_| SegmentTerminator::Default),
        map(take_while_m_n(1, 2, |c| c == '\r' || c == '\n'), |s: &str| {
            SegmentTerminator::Custom(s.to_string())
        }),
    ))(input)
}

fn msh(input: &str) -> IResult<&str, Message> {
    let (input, _) = tag("MSH")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, field_separator) = field_separator(input)?;
    let (input, _) = multispace0(input)?;
    let (input, encoding_characters) = encoding_characters(input)?;
    let (input, _) = multispace0(input)?;
    let (input, sending_application) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, sending_facility) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, receiving_application) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, receiving_facility) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, date_time_of_message) = take_while_m_n(1, 26, |c| c.is_alphanumeric() || c == '-' || c == ':')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, security) = take_while_m_n(1, 10, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, message_type) = take_while_m_n(1, 10, |c| c.is_alphanumeric() || c == '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, message_control_id) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, processing_id) = take_while_m_n(1, 10, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, version_id) = take_while_m_n(1, 10, |c| c.is_alphanumeric() || c == '.')(input)?;
    Ok((
        input,
        Message::MSH {
            field_separator,
            encoding_characters,
            sending_application: sending_application.to_string(),
            sending_facility: sending_facility.to_string(),
            receiving_application: receiving_application.to_string(),
            receiving_facility: receiving_facility.to_string(),
            date_time_of_message: date_time_of_message.to_string(),
            security: security.to_string(),
            message_type: message_type.to_string(),
            message_control_id: message_control_id.to_string(),
            processing_id: processing_id.to_string(),
            version_id: version_id.to_string(),
        },
    ))
}

fn pid(input: &str) -> IResult<&str, Message> {
    let (input, _) = tag("PID")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, set_id_patient_id) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, patient_id) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, patient_id_identifier) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, alternate_patient_id) = opt(take_while_m_n(1, 20, |c| c.is_alphanumeric()))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, patient_name) = take_while_m_n(1, 50, |c| c.is_alphanumeric() || c == '^')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, mothers_maiden_name) = opt(take_while_m_n(1, 20, |c| c.is_alphanumeric()))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, date_of_birth) = take_while_m_n(1, 8, |c| c.is_alphanumeric() || c == '-')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, sex) = take_while_m_n(1, 1, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, patient_alias) = opt(take_while_m_n(1, 50, |c| c.is_alphanumeric() || c == '^'))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, race) = opt(take_while_m_n(1, 20, |c| c.is_alphanumeric()))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, patient_address) = opt(take_while_m_n(1, 50, |c| c.is_alphanumeric() || c == '^'))(input)?;
    Ok((
        input,
        Message::PID {
            set_id_patient_id: set_id_patient_id.to_string(),
            patient_id: patient_id.to_string(),
            patient_id_identifier: patient_id_identifier.to_string(),
            alternate_patient_id: alternate_patient_id.map(|s| s.to_string()),
            patient_name: patient_name.to_string(),
            mothers_maiden_name: mothers_maiden_name.map(|s| s.to_string()),
            date_of_birth: date_of_birth.to_string(),
            sex: sex.to_string(),
            patient_alias: patient_alias.map(|s| s.to_string()),
            race: race.map(|s| s.to_string()),
            patient_address: patient_address.map(|s| s.to_string()),
        },
    ))
}

fn pv1(input: &str) -> IResult<&str, Message> {
    let (input, _) = tag("PV1")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, set_id_patient_visit) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, visit_number) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, patient_class) = take_while_m_n(1, 1, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, visit_number_identifier) = opt(take_while_m_n(1, 20, |c| c.is_alphanumeric()))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, financial_class) = opt(take_while_m_n(1, 20, |c| c.is_alphanumeric()))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, pre_admit_number) = opt(take_while_m_n(1, 20, |c| c.is_alphanumeric()))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, prior_patient_location) = opt(take_while_m_n(1, 20, |c| c.is_alphanumeric()))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, admitting_doctor) = opt(take_while_m_n(1, 50, |c| c.is_alphanumeric() || c == '^'))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, hospital_service) = opt(take_while_m_n(1, 20, |c| c.is_alphanumeric()))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, visit_type) = opt(take_while_m_n(1, 1, |c| c.is_alphanumeric()))(input)?;
    Ok((
        input,
        Message::PV1 {
            set_id_patient_visit: set_id_patient_visit.to_string(),
            visit_number: visit_number.to_string(),
            patient_class: patient_class.to_string(),
            visit_number_identifier: visit_number_identifier.map(|s| s.to_string()),
            financial_class: financial_class.map(|s| s.to_string()),
            pre_admit_number: pre_admit_number.map(|s| s.to_string()),
            prior_patient_location: prior_patient_location.map(|s| s.to_string()),
            admitting_doctor: admitting_doctor.map(|s| s.to_string()),
            hospital_service: hospital_service.map(|s| s.to_string()),
            visit_type: visit_type.map(|s| s.to_string()),
        },
    ))
}

fn obr(input: &str) -> IResult<&str, Message> {
    let (input, _) = tag("OBR")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, set_id_observation_request) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, placer_order_number) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, filler_order_number) = opt(take_while_m_n(1, 20, |c| c.is_alphanumeric()))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, universal_service_identifier) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, priority) = opt(take_while_m_n(1, 1, |c| c.is_alphanumeric()))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, requested_date_time) = opt(take_while_m_n(1, 26, |c| c.is_alphanumeric() || c == '-' || c == ':'))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, observation_date_time) = opt(take_while_m_n(1, 26, |c| c.is_alphanumeric() || c == '-' || c == ':'))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, observation_result_status) = opt(take_while_m_n(1, 1, |c| c.is_alphanumeric()))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, who_subject_definition) = opt(take_while_m_n(1, 20, |c| c.is_alphanumeric()))(input)?;
    Ok((
        input,
        Message::OBR {
            set_id_observation_request: set_id_observation_request.to_string(),
            placer_order_number: placer_order_number.to_string(),
            filler_order_number: filler_order_number.map(|s| s.to_string()),
            universal_service_identifier: universal_service_identifier.to_string(),
            priority: priority.map(|s| s.to_string()),
            requested_date_time: requested_date_time.map(|s| s.to_string()),
            observation_date_time: observation_date_time.map(|s| s.to_string()),
            observation_result_status: observation_result_status.map(|s| s.to_string()),
            who_subject_definition: who_subject_definition.map(|s| s.to_string()),
        },
    ))
}

fn obx(input: &str) -> IResult<&str, Message> {
    let (input, _) = tag("OBX")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, set_id_observation_result) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, observation_identifier) = take_while_m_n(1, 20, |c| c.is_alphanumeric())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, observation_sub_id) = opt(take_while_m_n(1