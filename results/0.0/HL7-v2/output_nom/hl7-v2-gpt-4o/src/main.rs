use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{alphanumeric1, char, digit1, line_ending},
    combinator::opt,
    multi::separated_list0,
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct MSH {
    field_separator: char,
    encoding_characters: String,
    sending_application: String,
    sending_facility: String,
    receiving_application: String,
    receiving_facility: String,
    date_time_of_message: String,
    message_type: String,
    message_control_id: String,
    processing_id: String,
    version_id: String,
}

#[derive(Debug)]
struct PID {
    set_id: Option<String>,
    patient_identifier_list: String,
    patient_name: String,
    date_time_of_birth: String,
    administrative_sex: Option<String>,
    patient_address: Option<String>,
}

fn parse_msh(input: &str) -> IResult<&str, MSH> {
    let (input, field_separator) = char('|')(input)?;
    let (input, encoding_characters) = alphanumeric1(input)?;
    let (input, sending_application) = alphanumeric1(input)?;
    let (input, sending_facility) = alphanumeric1(input)?;
    let (input, receiving_application) = alphanumeric1(input)?;
    let (input, receiving_facility) = alphanumeric1(input)?;
    let (input, date_time_of_message) = alphanumeric1(input)?;
    let (input, message_type) = alphanumeric1(input)?;
    let (input, message_control_id) = alphanumeric1(input)?;
    let (input, processing_id) = alphanumeric1(input)?;
    let (input, version_id) = alphanumeric1(input)?;

    Ok((
        input,
        MSH {
            field_separator,
            encoding_characters: encoding_characters.to_string(),
            sending_application: sending_application.to_string(),
            sending_facility: sending_facility.to_string(),
            receiving_application: receiving_application.to_string(),
            receiving_facility: receiving_facility.to_string(),
            date_time_of_message: date_time_of_message.to_string(),
            message_type: message_type.to_string(),
            message_control_id: message_control_id.to_string(),
            processing_id: processing_id.to_string(),
            version_id: version_id.to_string(),
        },
    ))
}

fn parse_pid(input: &str) -> IResult<&str, PID> {
    let (input, set_id) = opt(alphanumeric1)(input)?;
    let (input, patient_identifier_list) = alphanumeric1(input)?;
    let (input, patient_name) = alphanumeric1(input)?;
    let (input, date_time_of_birth) = alphanumeric1(input)?;
    let (input, administrative_sex) = opt(alphanumeric1)(input)?;
    let (input, patient_address) = opt(alphanumeric1)(input)?;

    Ok((
        input,
        PID {
            set_id: set_id.map(|s| s.to_string()),
            patient_identifier_list: patient_identifier_list.to_string(),
            patient_name: patient_name.to_string(),
            date_time_of_birth: date_time_of_birth.to_string(),
            administrative_sex: administrative_sex.map(|s| s.to_string()),
            patient_address: patient_address.map(|s| s.to_string()),
        },
    ))
}

fn parse_hl7(input: &str) -> IResult<&str, (MSH, Option<PID>)> {
    let (input, msh) = parse_msh(input)?;
    let (input, _) = line_ending(input)?;
    let (input, pid) = opt(parse_pid)(input)?;
    Ok((input, (msh, pid)))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match parse_hl7(&contents) {
        Ok((_, (msh, pid))) => {
            println!("Parsed MSH: {:?}", msh);
            if let Some(pid) = pid {
                println!("Parsed PID: {:?}", pid);
            }
        }
        Err(e) => eprintln!("Failed to parse HL7 message: {:?}", e),
    }

    Ok(())
}