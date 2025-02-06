use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{char, digit1, line_ending, space0},
    combinator::{map, opt},
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
    sending_application: Option<String>,
    sending_facility: Option<String>,
    receiving_application: Option<String>,
    receiving_facility: Option<String>,
    date_time_of_message: Option<String>,
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
    patient_identifier_list: Option<String>,
    alternate_patient_id: Option<String>,
    patient_name: Option<String>,
    mothers_maiden_name: Option<String>,
    date_time_of_birth: Option<String>,
    sex: Option<String>,
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
    patient_death_date_and_time: Option<String>,
    patient_death_indicator: Option<String>,
}

#[derive(Debug)]
struct PV1Segment {
    set_id: Option<String>,
    patient_class: Option<String>,
    assigned_patient_location: Option<String>,
    admission_type: Option<String>,
    preadmit_number: Option<String>,
    prior_patient_location: Option<String>,
    attending_doctor: Option<String>,
    referring_doctor: Option<String>,
    consulting_doctor: Option<String>,
    hospital_service: Option<String>,
    temporary_location: Option<String>,
    preadmit_test_indicator: Option<String>,
    readmission_indicator: Option<String>,
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
    admit_date_time: Option<String>,
    discharge_date_time: Option<String>,
    current_patient_balance: Option<String>,
    total_charges: Option<String>,
    total_adjustments: Option<String>,
    total_payments: Option<String>,
    alternate_visit_id: Option<String>,
}

#[derive(Debug)]
struct HL7Message {
    msh: MSHSegment,
    pid: Option<PIDSegment>,
    pv1: Option<PV1Segment>,
}

fn parse_msh_segment(input: &str) -> IResult<&str, MSHSegment> {
    let (input, _) = tag("MSH")(input)?;
    let (input, field_separator) = char('|')(input)?;
    let (input, encoding_characters) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, sending_application) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, sending_facility) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, receiving_application) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, receiving_facility) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, date_time_of_message) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, security) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, message_type) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, message_control_id) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, processing_id) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, version_id) = take_until("\r")(input)?;

    Ok((
        input,
        MSHSegment {
            field_separator,
            encoding_characters: encoding_characters.to_string(),
            sending_application: sending_application.map(|s| s.to_string()),
            sending_facility: sending_facility.map(|s| s.to_string()),
            receiving_application: receiving_application.map(|s| s.to_string()),
            receiving_facility: receiving_facility.map(|s| s.to_string()),
            date_time_of_message: date_time_of_message.map(|s| s.to_string()),
            security: security.map(|s| s.to_string()),
            message_type: message_type.to_string(),
            message_control_id: message_control_id.to_string(),
            processing_id: processing_id.to_string(),
            version_id: version_id.to_string(),
        },
    ))
}

fn parse_pid_segment(input: &str) -> IResult<&str, PIDSegment> {
    let (input, _) = tag("PID")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, set_id) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_id) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_identifier_list) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, alternate_patient_id) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_name) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, mothers_maiden_name) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, date_time_of_birth) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, sex) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_alias) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, race) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_address) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input)?;
    let (input, county_code) = opt(take_until("|"))(input)?;
    let (input, _) = char('|')(input