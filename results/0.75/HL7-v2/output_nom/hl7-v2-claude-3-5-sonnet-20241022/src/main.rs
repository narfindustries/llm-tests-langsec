use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{char, none_of},
    multi::{many0, many1},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::{env, fs};

#[derive(Debug)]
struct MSH {
    field_separator: char,
    encoding_chars: String,
    sending_app: String,
    sending_facility: String,
    receiving_app: String,
    receiving_facility: String,
    message_datetime: String,
    security: Option<String>,
    message_type: String,
    message_control_id: String,
    processing_id: String,
    version_id: String,
}

#[derive(Debug)]
struct PID {
    set_id: String,
    patient_id: String,
    patient_id_list: Vec<String>,
    alternate_id: Option<String>,
    patient_name: String,
    mother_maiden_name: Option<String>,
    birth_date: String,
    sex: char,
    patient_alias: Option<String>,
    race: Vec<String>,
    address: Vec<String>,
    county_code: Option<String>,
    phone_home: Vec<String>,
    phone_business: Vec<String>,
    language: Option<String>,
    marital_status: Option<String>,
    religion: Option<String>,
    patient_account: Option<String>,
}

#[derive(Debug)]
struct HL7Message {
    msh: MSH,
    pid: Option<PID>,
    segments: Vec<String>,
}

fn parse_field(input: &str) -> IResult<&str, String> {
    let (input, field) = take_until("|")(input)?;
    Ok((input, field.to_string()))
}

fn parse_optional_field(input: &str) -> IResult<&str, Option<String>> {
    let (input, field) = take_until("|")(input)?;
    if field.is_empty() {
        Ok((input, None))
    } else {
        Ok((input, Some(field.to_string())))
    }
}

fn parse_msh(input: &str) -> IResult<&str, MSH> {
    let (input, _) = tag("MSH")(input)?;
    let (input, field_separator) = char('|')(input)?;
    let (input, encoding_chars) = parse_field(input)?;
    let (input, sending_app) = preceded(char('|'), parse_field)(input)?;
    let (input, sending_facility) = preceded(char('|'), parse_field)(input)?;
    let (input, receiving_app) = preceded(char('|'), parse_field)(input)?;
    let (input, receiving_facility) = preceded(char('|'), parse_field)(input)?;
    let (input, message_datetime) = preceded(char('|'), parse_field)(input)?;
    let (input, security) = preceded(char('|'), parse_optional_field)(input)?;
    let (input, message_type) = preceded(char('|'), parse_field)(input)?;
    let (input, message_control_id) = preceded(char('|'), parse_field)(input)?;
    let (input, processing_id) = preceded(char('|'), parse_field)(input)?;
    let (input, version_id) = preceded(char('|'), parse_field)(input)?;

    Ok((
        input,
        MSH {
            field_separator,
            encoding_chars,
            sending_app,
            sending_facility,
            receiving_app,
            receiving_facility,
            message_datetime,
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
    let (input, set_id) = preceded(char('|'), parse_field)(input)?;
    let (input, patient_id) = preceded(char('|'), parse_field)(input)?;
    let (input, patient_id_list) = preceded(char('|'), many0(parse_field))(input)?;
    let (input, alternate_id) = preceded(char('|'), parse_optional_field)(input)?;
    let (input, patient_name) = preceded(char('|'), parse_field)(input)?;
    let (input, mother_maiden_name) = preceded(char('|'), parse_optional_field)(input)?;
    let (input, birth_date) = preceded(char('|'), parse_field)(input)?;
    let (input, sex) = preceded(char('|'), none_of("|"))(input)?;
    let (input, patient_alias) = preceded(char('|'), parse_optional_field)(input)?;
    let (input, race) = preceded(char('|'), many0(parse_field))(input)?;
    let (input, address) = preceded(char('|'), many0(parse_field))(input)?;
    let (input, county_code) = preceded(char('|'), parse_optional_field)(input)?;
    let (input, phone_home) = preceded(char('|'), many0(parse_field))(input)?;
    let (input, phone_business) = preceded(char('|'), many0(parse_field))(input)?;
    let (input, language) = preceded(char('|'), parse_optional_field)(input)?;
    let (input, marital_status) = preceded(char('|'), parse_optional_field)(input)?;
    let (input, religion) = preceded(char('|'), parse_optional_field)(input)?;
    let (input, patient_account) = preceded(char('|'), parse_optional_field)(input)?;

    Ok((
        input,
        PID {
            set_id,
            patient_id,
            patient_id_list,
            alternate_id,
            patient_name,
            mother_maiden_name,
            birth_date,
            sex,
            patient_alias,
            race,
            address,
            county_code,
            phone_home,
            phone_business,
            language,
            marital_status,
            religion,
            patient_account,
        },
    ))
}

fn parse_hl7_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, msh) = parse_msh(input)?;
    let (input, pid) = many0(parse_pid)(input)?;
    let (input, segments) = many0(parse_field)(input)?;

    Ok((
        input,
        HL7Message {
            msh,
            pid: pid.into_iter().next(),
            segments,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("Failed to read file");

    match parse_hl7_message(&contents) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse HL7 message: {:?}", e),
    }
}