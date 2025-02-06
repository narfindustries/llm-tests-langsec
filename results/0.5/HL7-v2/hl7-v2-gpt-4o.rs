use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{char, line_ending},
    combinator::opt,
    sequence::{terminated, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

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
}

#[derive(Debug)]
struct PID {
    patient_id: String,
    patient_name: String,
    date_of_birth: String,
    administrative_sex: String,
}

#[derive(Debug)]
struct HL7Message {
    msh: MSH,
    pid: Option<PID>,
}

fn parse_msh(input: &str) -> IResult<&str, MSH> {
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
    let (input, date_time_of_message) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, message_type) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, message_control_id) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, processing_id) = take_until("\r")(input)?;
    let (input, _) = line_ending(input)?;

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
        },
    ))
}

fn parse_pid(input: &str) -> IResult<&str, PID> {
    let (input, _) = tag("PID")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_id) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_name) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, date_of_birth) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, administrative_sex) = take_until("\r")(input)?;
    let (input, _) = line_ending(input)?;

    Ok((
        input,
        PID {
            patient_id: patient_id.to_string(),
            patient_name: patient_name.to_string(),
            date_of_birth: date_of_birth.to_string(),
            administrative_sex: administrative_sex.to_string(),
        },
    ))
}

fn parse_hl7(input: &str) -> IResult<&str, HL7Message> {
    let (input, msh) = parse_msh(input)?;
    let (input, pid) = opt(parse_pid)(input)?;

    Ok((input, HL7Message { msh, pid }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match parse_hl7(&contents) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Error parsing HL7 message: {:?}", e),
    }

    Ok(())
}