use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while},
    character::complete::{char, digit1, multispace0, multispace1},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, separated_list0, separated_pair},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::{
    collections::HashMap,
    env, fs,
    io::{self, Read},
    str,
};

// HL7 v2 message structure
#[derive(Debug, PartialEq)]
enum Message {
    MessageSegment(Vec<Segment>),
}

// HL7 v2 segment
#[derive(Debug, PartialEq)]
enum Segment {
    Msh(MshSegment),
    Evn(EvnSegment),
    Pid(PidSegment),
    // Add other segment types as needed
}

// HL7 v2 MSH segment
#[derive(Debug, PartialEq)]
struct MshSegment {
    field_separator: char,
    encoding_chars: String,
    sending_facility: String,
    sending_application: String,
    date_time_of_message: String,
    security: String,
    message_type: MessageType,
    message_control_id: String,
    processing_id: String,
    version_id: String,
    sequence_number: Option<String>,
    continuation_pointer: Option<String>,
    accept_acknowledgment_type: Option<String>,
    application_acknowledgment_type: Option<String>,
    country_code: Option<String>,
    character_set: Option<String>,
    principal_language_of_message: Option<String>,
}

// HL7 v2 EVN segment
#[derive(Debug, PartialEq)]
struct EvnSegment {
    event_type_code: String,
    recorded_date_time: String,
    date_time_planned_event: Option<String>,
    event_reason_code: Option<String>,
    operator_id: Option<String>,
    event_occurred: Option<String>,
}

// HL7 v2 PID segment
#[derive(Debug, PartialEq)]
struct PidSegment {
    set_id: String,
    patient_id: String,
    patient_identifier_list: Vec<PatientIdentifier>,
    patient_name: Vec<PatientName>,
    date_of_birth: Option<String>,
    sex: Option<String>,
    patient_alias: Option<Vec<PatientAlias>>,
    patient_address: Option<Vec<PatientAddress>>,
    county_code: Option<String>,
    phone_number_home: Option<String>,
    phone_number_business: Option<String>,
}

// HL7 v2 message type
#[derive(Debug, PartialEq)]
enum MessageType {
    A01,
    A02,
    A03,
    // Add other message types as needed
}

// HL7 v2 patient identifier
#[derive(Debug, PartialEq)]
struct PatientIdentifier {
    identifier_type: String,
    identifier: String,
}

// HL7 v2 patient name
#[derive(Debug, PartialEq)]
struct PatientName {
    name_type: String,
    family_name: String,
    given_name: String,
    suffix: Option<String>,
}

// HL7 v2 patient alias
#[derive(Debug, PartialEq)]
struct PatientAlias {
    alias_type: String,
    alias: String,
}

// HL7 v2 patient address
#[derive(Debug, PartialEq)]
struct PatientAddress {
    address_type: String,
    street_address: String,
    city: String,
    state_or_province: String,
    zip: String,
    country: Option<String>,
}

fn parse_message(input: &[u8]) -> IResult<&[u8], Message> {
    map(
        many0(parse_segment),
        |segments: Vec<Segment>| Message::MessageSegment(segments),
    )(input)
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Segment> {
    alt((parse_msh, parse_evn, parse_pid))(input)
}

fn parse_msh(input: &[u8]) -> IResult<&[u8], Segment> {
    map(
        tuple((
            tag("MSH"),
            multispace1,
            parse_field_separator,
            parse_encoding_chars,
            parse_sending_facility,
            parse_sending_application,
            parse_date_time_of_message,
            parse_security,
            parse_message_type,
            parse_message_control_id,
            parse_processing_id,
            parse_version_id,
            opt(parse_sequence_number),
            opt(parse_continuation_pointer),
            opt(parse_accept_acknowledgment_type),
            opt(parse_application_acknowledgment_type),
            opt(parse_country_code),
            opt(parse_character_set),
            opt(parse_principal_language_of_message),
        )),
        |(
            _,
            _,
            field_separator,
            encoding_chars,
            sending_facility,
            sending_application,
            date_time_of_message,
            security,
            message_type,
            message_control_id,
            processing_id,
            version_id,
            sequence_number,
            continuation_pointer,
            accept_acknowledgment_type,
            application_acknowledgment_type,
            country_code,
            character_set,
            principal_language_of_message,
        )| {
            Segment::Msh(MshSegment {
                field_separator,
                encoding_chars,
                sending_facility,
                sending_application,
                date_time_of_message,
                security,
                message_type,
                message_control_id,
                processing_id,
                version_id,
                sequence_number,
                continuation_pointer,
                accept_acknowledgment_type,
                application_acknowledgment_type,
                country_code,
                character_set,
                principal_language_of_message,
            })
        },
    )(input)
}

fn parse_evn(input: &[u8]) -> IResult<&[u8], Segment> {
    map(
        tuple((
            tag("EVN"),
            multispace1,
            parse_event_type_code,
            parse_recorded_date_time,
            opt(parse_date_time_planned_event),
            opt(parse_event_reason_code),
            opt(parse_operator_id),
            opt(parse_event_occurred),
        )),
        |(
            _,
            _,
            event_type_code,
            recorded_date_time,
            date_time_planned_event,
            event_reason_code,
            operator_id,
            event_occurred,
        )| {
            Segment::Evn(EvnSegment {
                event_type_code,
                recorded_date_time,
                date_time_planned_event,
                event_reason_code,
                operator_id,
                event_occurred,
            })
        },
    )(input)
}

fn parse_pid(input: &[u8]) -> IResult<&[u8], Segment> {
    map(
        tuple((
            tag("PID"),
            multispace1,
            parse_set_id,
            parse_patient_id,
            many0(parse_patient_identifier),
            many0(parse_patient_name),
            opt(parse_date_of_birth),
            opt(parse_sex),
            opt(many0(parse_patient_alias)),
            opt(many0(parse_patient_address)),
            opt(parse_county_code),
            opt(parse_phone_number_home),
            opt(parse_phone_number_business),
        )),
        |(
            _,
            _,
            set_id,
            patient_id,
            patient_identifier_list,
            patient_name_list,
            date_of_birth,
            sex,
            patient_alias_list,
            patient_address_list,
            county_code,
            phone_number_home,
            phone_number_business,
        )| {
            Segment::Pid(PidSegment {
                set_id,
                patient_id,
                patient_identifier_list,
                patient_name: patient_name_list,
                date_of_birth,
                sex,
                patient_alias: patient_alias_list,
                patient_address: patient_address_list,
                county_code,
                phone_number_home,
                phone_number_business,
            })
        },
    )(input)
}

fn parse_field_separator(input: &[u8]) -> IResult<&[u8], char> {
    map_res(take_while(|c| c != b'|' as u8), |field_separator: &[u8]| {
        char::from_utf8(field_separator[0])
    })(input)
}

fn parse_encoding_chars(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |encoding_chars: &[u8]| {
        String::from_utf8_lossy(encoding_chars).into_owned()
    })(input)
}

fn parse_sending_facility(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |sending_facility: &[u8]| {
        String::from_utf8_lossy(sending_facility).into_owned()
    })(input)
}

fn parse_sending_application(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |sending_application: &[u8]| {
        String::from_utf8_lossy(sending_application).into_owned()
    })(input)
}

fn parse_date_time_of_message(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |date_time_of_message: &[u8]| {
        String::from_utf8_lossy(date_time_of_message).into_owned()
    })(input)
}

fn parse_security(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |security: &[u8]| {
        String::from_utf8_lossy(security).into_owned()
    })(input)
}

fn parse_message_type(input: &[u8]) -> IResult<&[u8], MessageType> {
    alt((tag("A01"), tag("A02"), tag("A03")))(input)
}

fn parse_message_control_id(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |message_control_id: &[u8]| {
        String::from_utf8_lossy(message_control_id).into_owned()
    })(input)
}

fn parse_processing_id(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |processing_id: &[u8]| {
        String::from_utf8_lossy(processing_id).into_owned()
    })(input)
}

fn parse_version_id(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |version_id: &[u8]| {
        String::from_utf8_lossy(version_id).into_owned()
    })(input)
}

fn parse_sequence_number(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |sequence_number: &[u8]| {
        String::from_utf8_lossy(sequence_number).into_owned()
    })(input)
}

fn parse_continuation_pointer(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |continuation_pointer: &[u8]| {
        String::from_utf8_lossy(continuation_pointer).into_owned()
    })(input)
}

fn parse_accept_acknowledgment_type(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |accept_acknowledgment_type: &[u8]| {
        String::from_utf8_lossy(accept_acknowledgment_type).into_owned()
    })(input)
}

fn parse_application_acknowledgment_type(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |application_acknowledgment_type: &[u8]| {
        String::from_utf8_lossy(application_acknowledgment_type).into_owned()
    })(input)
}

fn parse_country_code(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |country_code: &[u8]| {
        String::from_utf8_lossy(country_code).into_owned()
    })(input)
}

fn parse_character_set(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |character_set: &[u8]| {
        String::from_utf8_lossy(character_set).into_owned()
    })(input)
}

fn parse_principal_language_of_message(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |principal_language_of_message: &[u8]| {
        String::from_utf8_lossy(principal_language_of_message).into_owned()
    })(input)
}

fn parse_event_type_code(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |event_type_code: &[u8]| {
        String::from_utf8_lossy(event_type_code).into_owned()
    })(input)
}

fn parse_recorded_date_time(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |recorded_date_time: &[u8]| {
        String::from_utf8_lossy(recorded_date_time).into_owned()
    })(input)
}

fn parse_date_time_planned_event(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |date_time_planned_event: &[u8]| {
        String::from_utf8_lossy(date_time_planned_event).into_owned()
    })(input)
}

fn parse_event_reason_code(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |event_reason_code: &[u8]| {
        String::from_utf8_lossy(event_reason_code).into_owned()
    })(input)
}

fn parse_operator_id(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |operator_id: &[u8]| {
        String::from_utf8_lossy(operator_id).into_owned()
    })(input)
}

fn parse_event_occurred(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |event_occurred: &[u8]| {
        String::from_utf8_lossy(event_occurred).into_owned()
    })(input)
}

fn parse_set_id(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |set_id: &[u8]| {
        String::from_utf8_lossy(set_id).into_owned()
    })(input)
}

fn parse_patient_id(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |patient_id: &[u8]| {
        String::from_utf8_lossy(patient_id).into_owned()
    })(input)
}

fn parse_patient_identifier(input: &[u8]) -> IResult<&[u8], PatientIdentifier> {
    map(
        tuple((parse_identifier_type, parse_identifier)),
        |(identifier_type, identifier)| PatientIdentifier {
            identifier_type,
            identifier,
        },
    )(input)
}

fn parse_identifier_type(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'^' as u8), |identifier_type: &[u8]| {
        String::from_utf8_lossy(identifier_type).into_owned()
    })(input)
}

fn parse_identifier(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |identifier: &[u8]| {
        String::from_utf8_lossy(identifier).into_owned()
    })(input)
}

fn parse_patient_name(input: &[u8]) -> IResult<&[u8], PatientName> {
    map(
        tuple((parse_name_type, parse_family_name, parse_given_name, opt(parse_suffix))),
        |(name_type, family_name, given_name, suffix)| PatientName {
            name_type,
            family_name,
            given_name,
            suffix,
        },
    )(input)
}

fn parse_name_type(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'^' as u8), |name_type: &[u8]| {
        String::from_utf8_lossy(name_type).into_owned()
    })(input)
}

fn parse_family_name(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'^' as u8), |family_name: &[u8]| {
        String::from_utf8_lossy(family_name).into_owned()
    })(input)
}

fn parse_given_name(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'^' as u8), |given_name: &[u8]| {
        String::from_utf8_lossy(given_name).into_owned()
    })(input)
}

fn parse_suffix(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |suffix: &[u8]| {
        String::from_utf8_lossy(suffix).into_owned()
    })(input)
}

fn parse_date_of_birth(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |date_of_birth: &[u8]| {
        String::from_utf8_lossy(date_of_birth).into_owned()
    })(input)
}

fn parse_sex(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8), |sex: &[u8]| {
        String::from_utf8_lossy(sex).into_owned()
    })(input)
}

fn parse_patient_alias(input: &[u8]) -> IResult<&[u8], PatientAlias> {
    map(
        tuple((parse_alias_type, parse_alias)),
        |(alias_type, alias)| PatientAlias { alias_type, alias },
    )(input)
}

fn parse_alias_type(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'^' as u8), |alias_type: &[u8]| {
        String::from_utf8_lossy(alias_type).into_owned()
    })(input)
}

fn parse_alias(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while(|c| c != b'|' as u8),