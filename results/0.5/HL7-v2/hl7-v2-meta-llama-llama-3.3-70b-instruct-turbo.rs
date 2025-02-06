use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, digit1, multispace0, multispace1},
    combinator::{map, map_res, opt, value},
    error::{context, Error, ErrorKind},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;

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
enum SubComponentSeparator {
    Default,
    Custom(char),
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
        message_type: MessageType,
        message_control_id: String,
        processing_id: ProcessingId,
        version_id: VersionId,
    },
    PID {
        set_id: String,
        patient_id: String,
        patient_id_identifier: String,
        alternate_patient_id: String,
        patient_name: String,
        mother_maiden_name: String,
        date_of_birth: String,
        sex: Sex,
        patient_alias: String,
        race: String,
        patient_address: String,
        county_code: String,
        phone_number_home: String,
        phone_number_business: String,
        primary_language: String,
        marital_status: MaritalStatus,
        religion: String,
        patient_account_number: String,
        ssn_number_patient: String,
        driver_s_license_number_patient: String,
        mother_s_maiden_name: String,
        citizenship: String,
        death_date: String,
        death_time: String,
        death_type_code: String,
        id_number: String,
        id_type_code: String,
        address_line_1: String,
        address_line_2: String,
        city: String,
        state_or_province: String,
        zip: String,
        country: String,
        phone_area_code: String,
        phone_number: String,
        phone_use_code: String,
        phone_equipment_type: String,
    },
    PV1 {
        set_id: String,
        visit_number: String,
        visit_indicator: VisitIndicator,
        admission_type: AdmissionType,
        preadmit_number: String,
        prior_patient_location: String,
        admission_date_time: String,
        discharge_date_time: String,
        visit_number_other: String,
        visit_indicator_other: VisitIndicator,
        admit_source: AdmitSource,
        admitting_doctor: String,
        visit_description: String,
        admit_reason: String,
        current_patient_balance: String,
        total_charges: String,
        total_adjustments: String,
        total_payments: String,
        alternate_visit_id: String,
        visit_priority_code: VisitPriorityCode,
    },
    OBR {
        set_id: String,
        placer_order_number: String,
        filler_order_number: String,
        universal_service_identifier: String,
        priority: Priority,
        requested_date_time: String,
        observation_date_time: String,
        observation_result_status: ObservationResultStatus,
        who_subject_filter: String,
        observation_result: String,
        charge_to_practice: String,
        diagnostic_serv_sec_id: String,
        result_status: ResultStatus,
        parent_result: String,
        observation_datetime: String,
        observation_end_date_time: String,
        collection_volume: String,
        collection_unit: String,
        collection_id: String,
    },
    OBX {
        set_id: String,
        value_type: ValueType,
        observation_identifier: String,
        observation_sub_id: String,
        observation_value: String,
        units: String,
        reference_range: String,
        abnormal_flags: AbnormalFlags,
        probability: String,
        nature_of_abnormal_test: NatureOfAbnormalTest,
        observation_result_status: ObservationResultStatus,
        effective_date_of_reference_range: String,
        user_defined_access_checks: String,
        date_last_observation_normal: String,
        user_defined_table: String,
        reserved_for_use_in_future_versions_of_hl7: String,
    },
}

#[derive(Debug, PartialEq)]
enum MessageType {
    ORU_R01,
    ORU_R02,
    ORU_R03,
    ORU_R04,
    ORU_R05,
    ORU_R06,
}

#[derive(Debug, PartialEq)]
enum ProcessingId {
    P,
    D,
    T,
}

#[derive(Debug, PartialEq)]
enum VersionId {
    V25,
    V26,
    V27,
    V28,
    V29,
    V210,
    V211,
    V212,
    V213,
    V214,
    V215,
    V216,
    V217,
    V218,
    V219,
    V220,
    V221,
    V222,
    V223,
    V224,
    V225,
}

#[derive(Debug, PartialEq)]
enum Sex {
    M,
    F,
    U,
    A,
    N,
    O,
}

#[derive(Debug, PartialEq)]
enum MaritalStatus {
    S,
    M,
    D,
    W,
    C,
    P,
}

#[derive(Debug, PartialEq)]
enum VisitIndicator {
    I,
    O,
}

#[derive(Debug, PartialEq)]
enum AdmissionType {
    E,
    U,
    C,
    N,
    R,
}

#[derive(Debug, PartialEq)]
enum AdmitSource {
    E,
    C,
    H,
    M,
    N,
    R,
    U,
}

#[derive(Debug, PartialEq)]
enum VisitPriorityCode {
    S,
    A,
    R,
    P,
}

#[derive(Debug, PartialEq)]
enum Priority {
    S,
    A,
    R,
    P,
}

#[derive(Debug, PartialEq)]
enum ObservationResultStatus {
    F,
    P,
    C,
    D,
}

#[derive(Debug, PartialEq)]
enum ResultStatus {
    F,
    P,
    C,
    D,
}

#[derive(Debug, PartialEq)]
enum ValueType {
    NM,
    TX,
    CE,
    CK,
    CN,
    CP,
    CX,
    DT,
    ED,
    FT,
    ID,
    IS,
    MO,
    NA,
    NM,
    PN,
    PT,
    RX,
    SI,
    SN,
    ST,
    TM,
    TN,
    TS,
    TX,
    UR,
    XAD,
    XCN,
    XON,
    XPN,
    XTN,
}

#[derive(Debug, PartialEq)]
enum AbnormalFlags {
    H,
    L,
    N,
}

#[derive(Debug, PartialEq)]
enum NatureOfAbnormalTest {
    A,
    N,
    R,
}

fn encoding_characters(input: &str) -> IResult<&str, EncodingCharacters> {
    alt((
        value(EncodingCharacters::Default, tag("^~\\&")),
        map(take_while1(|c| c != '^' && c != '~' && c != '\\'), |s: &str| {
            EncodingCharacters::Custom(s.to_string())
        }),
    ))(input)
}

fn field_separator(input: &str) -> IResult<&str, FieldSeparator> {
    alt((
        value(FieldSeparator::Default, char('|')),
        map(char('|'), |c: char| FieldSeparator::Custom(c)),
    ))(input)
}

fn component_separator(input: &str) -> IResult<&str, ComponentSeparator> {
    alt((
        value(ComponentSeparator::Default, char('^')),
        map(char('^'), |c: char| ComponentSeparator::Custom(c)),
    ))(input)
}

fn repetition_separator(input: &str) -> IResult<&str, RepetitionSeparator> {
    alt((
        value(RepetitionSeparator::Default, char('~')),
        map(char('~'), |c: char| RepetitionSeparator::Custom(c)),
    ))(input)
}

fn escape_character(input: &str) -> IResult<&str, EscapeCharacter> {
    alt((
        value(EscapeCharacter::Default, char('\\')),
        map(char('\\'), |c: char| EscapeCharacter::Custom(c)),
    ))(input)
}

fn sub_component_separator(input: &str) -> IResult<&str, SubComponentSeparator> {
    alt((
        value(SubComponentSeparator::Default, char('&')),
        map(char('&'), |c: char| SubComponentSeparator::Custom(c)),
    ))(input)
}

fn message_type(input: &str) -> IResult<&str, MessageType> {
    alt((
        value(MessageType::ORU_R01, tag("ORU^R01")),
        value(MessageType::ORU_R02, tag("ORU^R02")),
        value(MessageType::ORU_R03, tag("ORU^R03")),
        value(MessageType::ORU_R04, tag("ORU^R04")),
        value(MessageType::ORU_R05, tag("ORU^R05")),
        value(MessageType::ORU_R06, tag("ORU^R06")),
    ))(input)
}

fn processing_id(input: &str) -> IResult<&str, ProcessingId> {
    alt((
        value(ProcessingId::P, char('P')),
        value(ProcessingId::D, char('D')),
        value(ProcessingId::T, char('T')),
    ))(input)
}

fn version_id(input: &str) -> IResult<&str, VersionId> {
    alt((
        value(VersionId::V25, tag("2.5")),
        value(VersionId::V26, tag("2.6")),
        value(VersionId::V27, tag("2.7")),
        value(VersionId::V28, tag("2.8")),
        value(VersionId::V29, tag("2.9")),
        value(VersionId::V210, tag("2.10")),
        value(VersionId::V211, tag("2.11")),
        value(VersionId::V212, tag("2.12")),
        value(VersionId::V213, tag("2.13")),
        value(VersionId::V214, tag("2.14")),
        value(VersionId::V215, tag("2.15")),
        value(VersionId::V216, tag("2.16")),
        value(VersionId::V217, tag("2.17")),
        value(VersionId::V218, tag("2.18")),
        value(VersionId::V219, tag("2.19")),
        value(VersionId::V220, tag("2.20")),
        value(VersionId::V221, tag("2.21")),
        value(VersionId::V222, tag("2.22")),
        value(VersionId::V223, tag("2.23")),
        value(VersionId::V224, tag("2.24")),
        value(VersionId::V225, tag("2.25")),
    ))(input)
}

fn sex(input: &str) -> IResult<&str, Sex> {
    alt((
        value(Sex::M, char('M')),
        value(Sex::F, char('F')),
        value(Sex::U, char('U')),
        value(Sex::A, char('A')),
        value(Sex::N, char('N')),
        value(Sex::O, char('O')),
    ))(input)
}

fn marital_status(input: &str) -> IResult<&str, MaritalStatus> {
    alt((
        value(MaritalStatus::S, char('S')),
        value(MaritalStatus::M, char('M')),
        value(MaritalStatus::D, char('D')),
        value(MaritalStatus::W, char('W')),
        value(MaritalStatus::C, char('C')),
        value(MaritalStatus::P, char('P')),
    ))(input)
}

fn visit_indicator(input: &str) -> IResult<&str, VisitIndicator> {
    alt((
        value(VisitIndicator::I, char('I')),
        value(VisitIndicator::O, char('O')),
    ))(input)
}

fn admission_type(input: &str) -> IResult<&str, AdmissionType> {
    alt((
        value(AdmissionType::E, char('E')),
        value(AdmissionType::U, char('U')),
        value(AdmissionType::C, char('C')),
        value(AdmissionType::N, char('N')),
        value(AdmissionType::R, char('R')),
    ))(input)
}

fn admit_source(input: &str) -> IResult<&str, AdmitSource> {
    alt((
        value(AdmitSource::E, char('E')),
        value(AdmitSource::C, char('C')),
        value(AdmitSource::H, char('H')),
        value(AdmitSource::M, char('M')),
        value(AdmitSource::N, char('N')),
        value(AdmitSource::R, char('R')),
        value(AdmitSource::U, char('U')),
    ))(input)
}

fn visit_priority_code(input: &str) -> IResult<&str, VisitPriorityCode> {
    alt((
        value(VisitPriorityCode::S, char('S')),
        value(VisitPriorityCode::A, char('A')),
        value(VisitPriorityCode::R, char('R')),
        value(VisitPriorityCode::P, char('P')),
    ))(input)
}

fn priority(input: &str) -> IResult<&str, Priority> {
    alt((
        value(Priority::S, char('S')),
        value(Priority::A, char('A')),
        value(Priority::R, char('R')),
        value(Priority::P, char('P')),
    ))(input)
}

fn observation_result_status(input: &str) -> IResult<&str, ObservationResultStatus> {
    alt((
        value(ObservationResultStatus::F, char('F')),
        value(ObservationResultStatus::P, char('P')),
    ))(input)
}

fn result_status(input: &str) -> IResult<&str, ResultStatus> {
    alt((
        value(ResultStatus::F, char('F')),
        value(ResultStatus::P, char('P')),
    ))(input)
}

fn value_type(input: &str) -> IResult<&str, ValueType> {
    alt((
        value(ValueType::NM, tag("NM")),
        value(ValueType::TX, tag("TX")),
    ))(input)
}

fn abnormal_flags(input: &str) -> IResult<&str, AbnormalFlags> {
    alt((
        value(AbnormalFlags::H, char('H')),
        value(AbnormalFlags::L, char('L')),
        value(AbnormalFlags::N, char('N')),
    ))(input)
}

fn nature_of_abnormal_test(input: &str) -> IResult<&str, NatureOfAbnormalTest> {
    alt((
        value(NatureOfAbnormalTest::A, char('A')),
        value(NatureOfAbnormalTest::N, char('N')),
        value(NatureOfAbnormalTest::R, char('R')),
    ))(input)
}

fn msh(input: &str) -> IResult<&str, Message> {
    let (input, _) = tag("MSH")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, field_separator) = field_separator(input)?;
    let (input, _) = multispace0(input)?;
    let (input, encoding_characters) = encoding_characters(input)?;
    let (input, _) = multispace0(input)?;
    let (input, sending_application) = take_while1(|c| c != '|')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, sending_facility) = take_while1(|c| c != '|')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, receiving_application) = take_while1(|c| c != '|')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, receiving_facility) = take_while1(|c| c != '|')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, date_time_of_message) = take_while1(|c| c != '|')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, security) = take_while1(|c| c != '|')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, message_type) = message_type(input)?;
    let (input, _) = multispace0(input)?;
    let (input, message_control_id) = take_while1(|c| c != '|')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, processing_id) = processing_id(input)?;
    let (input, _) = multispace0(input)?;
    let (input, version_id) = version_id(input)?;
    Ok((input, Message::MSH {
        field_separator,
        encoding_characters,
        sending_application: sending_application.to_string(),
        sending_facility: sending_facility.to_string(),
        receiving_application: receiving_application.to_string(),
        receiving_facility: receiving_facility.to_string(),
        date_time_of_message: date_time_of_message.to_string(),
        security: security.to_string(),
        message_type,
        message_control_id: message_control_id.to_string(),
        processing_id,
        version_id,
    }))
}

fn pid(input: &str) -> IResult<&str, Message> {
    let (input, _) = tag("PID")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, set_id) = take_while1(|c| c != '|')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, patient_id) = take_while1(|c| c != '|')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, patient_id_identifier) = take_while1(|c| c != '|')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, alternate_patient_id) = take_while1(|c| c != '|')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, patient_name) = take_while1(|c| c != '|')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, mother_maiden_name) = take_while1(|c| c != '|')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, date_of_birth) = take_while1(|c| c != '|')(