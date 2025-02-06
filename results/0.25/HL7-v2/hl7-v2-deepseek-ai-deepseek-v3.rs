use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{char, digit1, line_ending, space0},
    combinator::{map, opt, recognize},
    multi::{many0, separated_list1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct MSHSegment {
    field_separator: char,
    encoding_characters: String,
    sending_application: String,
    sending_facility: String,
    receiving_application: String,
    receiving_facility: String,
    datetime_of_message: String,
    security: Option<String>,
    message_type: String,
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

#[derive(Debug)]
struct PIDSegment {
    set_id: String,
    patient_id: String,
    patient_identifier_list: Vec<String>,
    alternate_patient_id: Option<String>,
    patient_name: String,
    mothers_maiden_name: Option<String>,
    datetime_of_birth: String,
    sex: String,
    patient_alias: Option<String>,
    race: Option<String>,
    patient_address: Option<String>,
    county_code: Option<String>,
    phone_number_home: Option<String>,
    phone_number_business: Option<String>,
    primary_language: Option<String>,
    marital_status: Option<String>,
    religion: Option<String>,
    patient_account_number: Option<String>,
    ssn_number: Option<String>,
    drivers_license_number: Option<String>,
    mothers_identifier: Option<String>,
    ethnic_group: Option<String>,
    birth_place: Option<String>,
    multiple_birth_indicator: Option<String>,
    birth_order: Option<String>,
    citizenship: Option<String>,
    veterans_military_status: Option<String>,
    nationality: Option<String>,
    patient_death_datetime: Option<String>,
    patient_death_indicator: Option<String>,
}

#[derive(Debug)]
struct PV1Segment {
    set_id: String,
    patient_class: String,
    assigned_patient_location: String,
    admission_type: Option<String>,
    preadmit_number: Option<String>,
    prior_patient_location: Option<String>,
    attending_doctor: Option<String>,
    referring_doctor: Option<String>,
    consulting_doctor: Option<String>,
    hospital_service: Option<String>,
    temporary_location: Option<String>,
    preadmit_test_indicator: Option<String>,
    re_admission_indicator: Option<String>,
    admit_source: Option<String>,
    ambulatory_status: Option<String>,
    vip_indicator: Option<String>,
    admitting_doctor: Option<String>,
    patient_type: Option<String>,
    visit_number: Option<String>,
    financial_class: Option<String>,
    charge_price_indicator: Option<String>,
    courtesy_code: Option<String>,
    credit_rating: Option<String>,
    contract_code: Option<String>,
    contract_effective_date: Option<String>,
    contract_amount: Option<String>,
    contract_period: Option<String>,
    interest_code: Option<String>,
    transfer_to_bad_debt_code: Option<String>,
    transfer_to_bad_debt_date: Option<String>,
    bad_debt_agency_code: Option<String>,
    bad_debt_transfer_amount: Option<String>,
    bad_debt_recovery_amount: Option<String>,
    delete_account_indicator: Option<String>,
    delete_account_date: Option<String>,
    discharge_disposition: Option<String>,
    discharged_to_location: Option<String>,
    diet_type: Option<String>,
    servicing_facility: Option<String>,
    bed_status: Option<String>,
    account_status: Option<String>,
    pending_location: Option<String>,
    prior_temporary_location: Option<String>,
    admit_datetime: Option<String>,
    discharge_datetime: Option<String>,
    current_patient_balance: Option<String>,
    total_charges: Option<String>,
    total_adjustments: Option<String>,
}

fn parse_msh_segment(input: &str) -> IResult<&str, MSHSegment> {
    let (input, _) = tag("MSH")(input)?;
    let (input, field_separator) = char('|')(input)?;
    let (input, encoding_characters) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, sending_application) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, sending_facility) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, receiving_application) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, receiving_facility) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, datetime_of_message) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, security) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, message_type) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, message_control_id) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, processing_id) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, version_id) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, sequence_number) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, continuation_pointer) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, accept_acknowledgment_type) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, application_acknowledgment_type) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, country_code) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, character_set) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, principal_language_of_message) = opt(take_until("|"))(input)?;
    let (input, _) = line_ending(input)?;

    Ok((
        input,
        MSHSegment {
            field_separator,
            encoding_characters: encoding_characters.to_string(),
            sending_application: sending_application.to_string(),
            sending_facility: sending_facility.to_string(),
            receiving_application: receiving_application.to_string(),
            receiving_facility: receiving_facility.to_string(),
            datetime_of_message: datetime_of_message.to_string(),
            security: security.map(|s| s.to_string()),
            message_type: message_type.to_string(),
            message_control_id: message_control_id.to_string(),
            processing_id: processing_id.to_string(),
            version_id: version_id.to_string(),
            sequence_number: sequence_number.map(|s| s.to_string()),
            continuation_pointer: continuation_pointer.map(|s| s.to_string()),
            accept_acknowledgment_type: accept_acknowledgment_type.map(|s| s.to_string()),
            application_acknowledgment_type: application_acknowledgment_type.map(|s| s.to_string()),
            country_code: country_code.map(|s| s.to_string()),
            character_set: character_set.map(|s| s.to_string()),
            principal_language_of_message: principal_language_of_message.map(|s| s.to_string()),
        },
    ))
}

fn parse_pid_segment(input: &str) -> IResult<&str, PIDSegment> {
    let (input, _) = tag("PID")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, set_id) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_id) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_identifier_list) = separated_list