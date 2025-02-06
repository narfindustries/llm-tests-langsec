use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{char, digit1, line_ending, none_of},
    combinator::{map, opt, recognize},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct MSH {
    field_separator: char,
    encoding_characters: String,
    sending_application: String,
    sending_facility: String,
    receiving_application: String,
    receiving_facility: String,
    date_time_of_message: String,
    security: Option<String>,
    message_type: String,
    message_control_id: String,
    processing_id: String,
    version_id: String,
}

#[derive(Debug)]
struct PID {
    set_id: String,
    patient_id: Option<String>,
    patient_identifier_list: Option<String>,
    patient_name: Option<String>,
    date_of_birth: Option<String>,
    sex: Option<String>,
    patient_address: Option<String>,
    phone_number: Option<String>,
    patient_account_number: Option<String>,
}

#[derive(Debug)]
struct PV1 {
    set_id: String,
    patient_class: String,
    assigned_patient_location: Option<String>,
    attending_doctor: Option<String>,
    referring_doctor: Option<String>,
    consulting_doctor: Option<String>,
    hospital_service: Option<String>,
    discharge_disposition: Option<String>,
}

#[derive(Debug)]
struct OBR {
    set_id: String,
    placer_order_number: Option<String>,
    filler_order_number: Option<String>,
    universal_service_id: String,
    observation_date_time: String,
    ordering_provider: Option<String>,
    result_status: String,
}

#[derive(Debug)]
struct OBX {
    set_id: String,
    value_type: String,
    observation_identifier: String,
    observation_value: String,
    units: Option<String>,
    abnormal_flags: Option<String>,
    observation_result_status: String,
}

#[derive(Debug)]
struct HL7Message {
    msh: MSH,
    pid: PID,
    pv1: PV1,
    obr: Vec<OBR>,
    obx: Vec<OBX>,
}

fn parse_field(input: &str) -> IResult<&str, String> {
    map(take_until("|"), |s: &str| s.to_string())(input)
}

fn parse_optional_field(input: &str) -> IResult<&str, Option<String>> {
    opt(parse_field)(input)
}

fn parse_msh(input: &str) -> IResult<&str, MSH> {
    let (input, _) = tag("MSH")(input)?;
    let (input, field_separator) = none_of("|")(input)?;
    let (input, encoding_characters) = parse_field(input)?;
    let (input, sending_application) = parse_field(input)?;
    let (input, sending_facility) = parse_field(input)?;
    let (input, receiving_application) = parse_field(input)?;
    let (input, receiving_facility) = parse_field(input)?;
    let (input, date_time_of_message) = parse_field(input)?;
    let (input, security) = parse_optional_field(input)?;
    let (input, message_type) = parse_field(input)?;
    let (input, message_control_id) = parse_field(input)?;
    let (input, processing_id) = parse_field(input)?;
    let (input, version_id) = parse_field(input)?;

    Ok((
        input,
        MSH {
            field_separator,
            encoding_characters,
            sending_application,
            sending_facility,
            receiving_application,
            receiving_facility,
            date_time_of_message,
            security,
            message_type,
            message_control_id,
            processing_id,
            version_id,
        },
    ))
}

fn parse_pid(input: &str) -> IResult<&str, PID> {
    let (input, _) = tag("PID")(input)?;
    let (input, set_id) = parse_field(input)?;
    let (input, patient_id) = parse_optional_field(input)?;
    let (input, patient_identifier_list) = parse_optional_field(input)?;
    let (input, patient_name) = parse_optional_field(input)?;
    let (input, date_of_birth) = parse_optional_field(input)?;
    let (input, sex) = parse_optional_field(input)?;
    let (input, patient_address) = parse_optional_field(input)?;
    let (input, phone_number) = parse_optional_field(input)?;
    let (input, patient_account_number) = parse_optional_field(input)?;

    Ok((
        input,
        PID {
            set_id,
            patient_id,
            patient_identifier_list,
            patient_name,
            date_of_birth,
            sex,
            patient_address,
            phone_number,
            patient_account_number,
        },
    ))
}

fn parse_pv1(input: &str) -> IResult<&str, PV1> {
    let (input, _) = tag("PV1")(input)?;
    let (input, set_id) = parse_field(input)?;
    let (input, patient_class) = parse_field(input)?;
    let (input, assigned_patient_location) = parse_optional_field(input)?;
    let (input, attending_doctor) = parse_optional_field(input)?;
    let (input, referring_doctor) = parse_optional_field(input)?;
    let (input, consulting_doctor) = parse_optional_field(input)?;
    let (input, hospital_service) = parse_optional_field(input)?;
    let (input, discharge_disposition) = parse_optional_field(input)?;

    Ok((
        input,
        PV1 {
            set_id,
            patient_class,
            assigned_patient_location,
            attending_doctor,
            referring_doctor,
            consulting_doctor,
            hospital_service,
            discharge_disposition,
        },
    ))
}

fn parse_obr(input: &str) -> IResult<&str, OBR> {
    let (input, _) = tag("OBR")(input)?;
    let (input, set_id) = parse_field(input)?;
    let (input, placer_order_number) = parse_optional_field(input)?;
    let (input, filler_order_number) = parse_optional_field(input)?;
    let (input, universal_service_id) = parse_field(input)?;
    let (input, observation_date_time) = parse_field(input)?;
    let (input, ordering_provider) = parse_optional_field(input)?;
    let (input, result_status) = parse_field(input)?;

    Ok((
        input,
        OBR {
            set_id,
            placer_order_number,
            filler_order_number,
            universal_service_id,
            observation_date_time,
            ordering_provider,
            result_status,
        },
    ))
}

fn parse_obx(input: &str) -> IResult<&str, OBX> {
    let (input, _) = tag("OBX")(input)?;
    let (input, set_id) = parse_field(input)?;
    let (input, value_type) = parse_field(input)?;
    let (input, observation_identifier) = parse_field(input)?;
    let (input, observation_value) = parse_field(input)?;
    let (input, units) = parse_optional_field(input)?;
    let (input, abnormal_flags) = parse_optional_field(input)?;
    let (input, observation_result_status) = parse_field(input)?;

    Ok((
        input,
        OBX {
            set_id,
            value_type,
            observation_identifier,
            observation_value,
            units,
            abnormal_flags,
            observation_result_status,
        },
    ))
}

fn parse_hl7_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, msh) = parse_msh(input)?;
    let (input, pid) = parse_pid(input)?;
    let (input, pv1) = parse_pv1(input)?;
    let (input, obr) = many1(parse_obr)(input)?;
    let (input, obx) = many1(parse_obx)(input)?;

    Ok((
        input,
        HL7Message {
            msh,
            pid,
            pv1,
            obr,
            obx,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match parse_hl7_message(&contents) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse HL7 message: {:?}", e),
    }

    Ok(())
}