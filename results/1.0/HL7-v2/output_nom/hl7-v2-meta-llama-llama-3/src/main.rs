use nom::branch::alt;
use nom::bytes::complete::{tag, take_while_m_n};
use nom::character::complete::{char};
use nom::combinator::{map, opt};
use nom::error::{context, ParseError};
use nom::sequence::{tuple};
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::{Read, BufReader};
use std::path::Path;

#[derive(Debug, PartialEq)]
enum EncodingCharacters {
    Caret,
    Tilde,
    Ampersand,
}

impl EncodingCharacters {
    fn parse(input: &str) -> IResult<&str, EncodingCharacters> {
        alt((tag("^"), tag("~"), tag("&")))(input)
            .map(|(input, c)| match c {
                "^" => (input, EncodingCharacters::Caret),
                "~" => (input, EncodingCharacters::Tilde),
                "&" => (input, EncodingCharacters::Ampersand),
                _ => unreachable!(),
            })
    }
}

#[derive(Debug, PartialEq)]
enum FieldSeparator {
    Pipe,
}

impl FieldSeparator {
    fn parse(input: &str) -> IResult<&str, FieldSeparator> {
        char('|')(input).map(|(input, _)| (input, FieldSeparator::Pipe))
    }
}

#[derive(Debug, PartialEq)]
enum SegmentIdentifier {
    MSH,
    EVN,
    PID,
    PV1,
    ORC,
    OBR,
    OBX,
    RXE,
    RXC,
    RXD,
    RXG,
    RXR,
}

impl SegmentIdentifier {
    fn parse(input: &str) -> IResult<&str, SegmentIdentifier> {
        alt((
            tag("MSH"),
            tag("EVN"),
            tag("PID"),
            tag("PV1"),
            tag("ORC"),
            tag("OBR"),
            tag("OBX"),
            tag("RXE"),
            tag("RXC"),
            tag("RXD"),
            tag("RXG"),
            tag("RXR"),
        ))(input)
        .map(|(input, ident)| match ident {
            "MSH" => (input, SegmentIdentifier::MSH),
            "EVN" => (input, SegmentIdentifier::EVN),
            "PID" => (input, SegmentIdentifier::PID),
            "PV1" => (input, SegmentIdentifier::PV1),
            "ORC" => (input, SegmentIdentifier::ORC),
            "OBR" => (input, SegmentIdentifier::OBR),
            "OBX" => (input, SegmentIdentifier::OBX),
            "RXE" => (input, SegmentIdentifier::RXE),
            "RXC" => (input, SegmentIdentifier::RXC),
            "RXD" => (input, SegmentIdentifier::RXD),
            "RXG" => (input, SegmentIdentifier::RXG),
            "RXR" => (input, SegmentIdentifier::RXR),
            _ => unreachable!(),
        })
    }
}

#[derive(Debug, PartialEq)]
enum MessageType {
    MSG,
    ACK,
}

impl MessageType {
    fn parse(input: &str) -> IResult<&str, MessageType> {
        alt((tag("MSG"), tag("ACK")))(input)
            .map(|(input, msg_type)| match msg_type {
                "MSG" => (input, MessageType::MSG),
                "ACK" => (input, MessageType::ACK),
                _ => unreachable!(),
            })
    }
}

#[derive(Debug, PartialEq)]
enum TriggerEvent {
    AdtA01,
    OrmO01,
}

impl TriggerEvent {
    fn parse(input: &str) -> IResult<&str, TriggerEvent> {
        alt((tag("ADT_A01"), tag("ORM_O01")))(input)
            .map(|(input, event)| match event {
                "ADT_A01" => (input, TriggerEvent::AdtA01),
                "ORM_O01" => (input, TriggerEvent::OrmO01),
                _ => unreachable!(),
            })
    }
}

#[derive(Debug, PartialEq)]
struct MessageHeader {
    field_separator: FieldSeparator,
    encoding_characters: EncodingCharacters,
    sending_facility: String,
    receiving_facility: String,
    date_time: String,
    security: Option<String>,
    message_type: MessageType,
    trigger_event: TriggerEvent,
    message_control_id: String,
    processing_id: String,
}

impl MessageHeader {
    fn parse(input: &str) -> IResult<&str, MessageHeader> {
        context(
            "Message Header",
            tuple((
                FieldSeparator::parse,
                EncodingCharacters::parse,
                take_while_m_n(1, 20, |c: char| c.is_alphanumeric() || c == '_'),
                take_while_m_n(1, 20, |c: char| c.is_alphanumeric() || c == '_'),
                take_while_m_n(1, 14, |c: char| c.is_alphanumeric()),
                opt(take_while_m_n(1, 2, |c: char| c.is_alphanumeric())),
                MessageType::parse,
                TriggerEvent::parse,
                take_while_m_n(1, 20, |c: char| c.is_alphanumeric() || c == '_'),
                take_while_m_n(1, 2, |c: char| c.is_alphanumeric()),
            )),
        )(input)
        .map(
            |(input, (
                field_separator,
                encoding_characters,
                sending_facility,
                receiving_facility,
                date_time,
                security,
                message_type,
                trigger_event,
                message_control_id,
                processing_id,
            ))| {
                (
                    input,
                    MessageHeader {
                        field_separator,
                        encoding_characters,
                        sending_facility: sending_facility.to_string(),
                        receiving_facility: receiving_facility.to_string(),
                        date_time: date_time.to_string(),
                        security: security.map(|s| s.to_string()),
                        message_type,
                        trigger_event,
                        message_control_id: message_control_id.to_string(),
                        processing_id: processing_id.to_string(),
                    },
                )
            },
        )
    }
}

#[derive(Debug, PartialEq)]
struct EventType {
    event_type_code: String,
    recorded_date_time: String,
    date_time_planned_event: Option<String>,
    event_reason_code: Option<String>,
    operator_id: Option<String>,
}

impl EventType {
    fn parse(input: &str) -> IResult<&str, EventType> {
        context(
            "Event Type",
            tuple((
                take_while_m_n(1, 3, |c: char| c.is_alphanumeric()),
                take_while_m_n(1, 14, |c: char| c.is_alphanumeric()),
                opt(take_while_m_n(1, 14, |c: char| c.is_alphanumeric())),
                opt(take_while_m_n(1, 2, |c: char| c.is_alphanumeric())),
                opt(take_while_m_n(1, 20, |c: char| c.is_alphanumeric() || c == '_')),
            )),
        )(input)
        .map(
            |(input, (
                event_type_code,
                recorded_date_time,
                date_time_planned_event,
                event_reason_code,
                operator_id,
            ))| {
                (
                    input,
                    EventType {
                        event_type_code: event_type_code.to_string(),
                        recorded_date_time: recorded_date_time.to_string(),
                        date_time_planned_event: date_time_planned_event.map(|s| s.to_string()),
                        event_reason_code: event_reason_code.map(|s| s.to_string()),
                        operator_id: operator_id.map(|s| s.to_string()),
                    },
                )
            },
        )
    }
}

#[derive(Debug, PartialEq)]
struct PatientInformation {
    set_id_patient_id: String,
    patient_id: String,
    patient_name: String,
    date_of_birth: String,
    sex: String,
    patient_address: String,
}

impl PatientInformation {
    fn parse(input: &str) -> IResult<&str, PatientInformation> {
        context(
            "Patient Information",
            tuple((
                take_while_m_n(1, 20, |c: char| c.is_alphanumeric() || c == '_'),
                take_while_m_n(1, 20, |c: char| c.is_alphanumeric() || c == '_'),
                take_while_m_n(1, 50, |c: char| c.is_alphanumeric() || c == '_'),
                take_while_m_n(1, 8, |c: char| c.is_alphanumeric()),
                take_while_m_n(1, 1, |c: char| c.is_alphanumeric()),
                take_while_m_n(1, 50, |c: char| c.is_alphanumeric() || c == '_'),
            )),
        )(input)
        .map(
            |(input, (
                set_id_patient_id,
                patient_id,
                patient_name,
                date_of_birth,
                sex,
                patient_address,
            ))| {
                (
                    input,
                    PatientInformation {
                        set_id_patient_id: set_id_patient_id.to_string(),
                        patient_id: patient_id.to_string(),
                        patient_name: patient_name.to_string(),
                        date_of_birth: date_of_birth.to_string(),
                        sex: sex.to_string(),
                        patient_address: patient_address.to_string(),
                    },
                )
            },
        )
    }
}

#[derive(Debug, PartialEq)]
struct VisitInformation {
    visit_number: String,
    patient_class: String,
    admitting_doctor: String,
    discharge_disposition: String,
}

impl VisitInformation {
    fn parse(input: &str) -> IResult<&str, VisitInformation> {
        context(
            "Visit Information",
            tuple((
                take_while_m_n(1, 20, |c: char| c.is_alphanumeric() || c == '_'),
                take_while_m_n(1, 1, |c: char| c.is_alphanumeric()),
                take_while_m_n(1, 50, |c: char| c.is_alphanumeric() || c == '_'),
                take_while_m_n(1, 2, |c: char| c.is_alphanumeric()),
            )),
        )(input)
        .map(
            |(input, (
                visit_number,
                patient_class,
                admitting_doctor,
                discharge_disposition,
            ))| {
                (
                    input,
                    VisitInformation {
                        visit_number: visit_number.to_string(),
                        patient_class: patient_class.to_string(),
                        admitting_doctor: admitting_doctor.to_string(),
                        discharge_disposition: discharge_disposition.to_string(),
                    },
                )
            },
        )
    }
}

#[derive(Debug, PartialEq)]
struct OrderInformation {
    order_control: String,
    order_id: String,
    order_type: String,
    order_priority: String,
}

impl OrderInformation {
    fn parse(input: &str) -> IResult<&str, OrderInformation> {
        context(
            "Order Information",
            tuple((
                take_while_m_n(1, 2, |c: char| c.is_alphanumeric()),
                take_while_m_n(1, 20, |c: char| c.is_alphanumeric() || c == '_'),
                take_while_m_n(1, 3, |c: char| c.is_alphanumeric()),
                take_while_m_n(1, 10, |c: char| c.is_alphanumeric()),
            )),
        )(input)
        .map(
            |(input, (
                order_control,
                order_id,
                order_type,
                order_priority,
            ))| {
                (
                    input,
                    OrderInformation {
                        order_control: order_control.to_string(),
                        order_id: order_id.to_string(),
                        order_type: order_type.to_string(),
                        order_priority: order_priority.to_string(),
                    },
                )
            },
        )
    }
}

#[derive(Debug, PartialEq)]
struct ObservationRequest {
    observation_request: String,
    observation_id: String,
    observation_result: String,
    units: String,
    reference_range: String,
    abnormal_flags: String,
}

impl ObservationRequest {
    fn parse(input: &str) -> IResult<&str, ObservationRequest> {
        context(
            "Observation Request",
            tuple((
                take_while_m_n(1, 20, |c: char| c.is_alphanumeric() || c == '_'),
                take_while_m_n(1, 20, |c: char| c.is_alphanumeric() || c == '_'),
                take_while_m_n(1, 50, |c: char| c.is_alphanumeric() || c == '_'),
                take_while_m_n(1, 10, |c: char| c.is_alphanumeric()),
                take_while_m_n(1, 10, |c: char| c.is_alphanumeric()),
                take_while_m_n(1, 1, |c: char| c.is_alphanumeric()),
            )),
        )(input)
        .map(
            |(input, (
                observation_request,
                observation_id,
                observation_result,
                units,
                reference_range,
                abnormal_flags,
            ))| {
                (
                    input,
                    ObservationRequest {
                        observation_request: observation_request.to_string(),
                        observation_id: observation_id.to_string(),
                        observation_result: observation_result.to_string(),
                        units: units.to_string(),
                        reference_range: reference_range.to_string(),
                        abnormal_flags: abnormal_flags.to_string(),
                    },
                )
            },
        )
    }
}

#[derive(Debug, PartialEq)]
enum HL7Message {
    MessageHeader(MessageHeader),
    EventType(EventType),
    PatientInformation(PatientInformation),
    VisitInformation(VisitInformation),
    OrderInformation(OrderInformation),
    ObservationRequest(ObservationRequest),
}

impl HL7Message {
    fn parse(input: &str) -> IResult<&str, HL7Message> {
        alt((
            map(MessageHeader::parse, HL7Message::MessageHeader),
            map(EventType::parse, HL7Message::EventType),
            map(PatientInformation::parse, HL7Message::PatientInformation),
            map(VisitInformation::parse, HL7Message::VisitInformation),
            map(OrderInformation::parse, HL7Message::OrderInformation),
            map(ObservationRequest::parse, HL7Message::ObservationRequest),
        ))(input)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <hl7_file>", args[0]);
        return;
    }
    let path = Path::new(&args[1]);
    let file = File::open(path).expect("Failed to open file");
    let mut reader = BufReader::new(file);
    let mut input = String::new();
    reader.read_to_string(&mut input).expect("Failed to read file");
    let result = HL7Message::parse(&input);
    match result {
        Ok((_, msg)) => println!("{:?}", msg),
        Err(err) => println!("Error parsing HL7 message: {:?}", err),
    }
}