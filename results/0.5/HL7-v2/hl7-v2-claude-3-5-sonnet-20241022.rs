use nom::{
    bytes::complete::{tag, take_until},
    character::complete::char,
    multi::many0,
    sequence::tuple,
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
    datetime: String,
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
    sex: String,
}

#[derive(Debug)]
struct HL7Message {
    msh: MSH,
    pid: Option<PID>,
}

fn parse_msh(input: &str) -> IResult<&str, MSH> {
    let (input, sep) = char('|')(input)?;
    let (input, _) = tag("|^~\\&|")(input)?;
    let (input, sending_app) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, sending_facility) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, receiving_app) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, receiving_facility) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, datetime) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, security) = take_until("|")(input)?;
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
        MSH {
            field_separator: sep,
            encoding_chars: "^~\\&".to_string(),
            sending_app: sending_app.to_string(),
            sending_facility: sending_facility.to_string(),
            receiving_app: receiving_app.to_string(),
            receiving_facility: receiving_facility.to_string(),
            datetime: datetime.to_string(),
            security: if security.is_empty() {
                None
            } else {
                Some(security.to_string())
            },
            message_type: message_type.to_string(),
            message_control_id: message_control_id.to_string(),
            processing_id: processing_id.to_string(),
            version_id: version_id.to_string(),
        },
    ))
}

fn parse_pid(input: &str) -> IResult<&str, PID> {
    let (input, _) = tag("PID|")(input)?;
    let (input, set_id) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_id) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_id_list_str) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, alternate_id) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_name) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, mother_maiden_name) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, birth_date) = take_until("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, sex) = take_until("\r")(input)?;

    let patient_id_list = patient_id_list_str
        .split('~')
        .map(String::from)
        .collect::<Vec<String>>();

    Ok((
        input,
        PID {
            set_id: set_id.to_string(),
            patient_id: patient_id.to_string(),
            patient_id_list,
            alternate_id: if alternate_id.is_empty() {
                None
            } else {
                Some(alternate_id.to_string())
            },
            patient_name: patient_name.to_string(),
            mother_maiden_name: if mother_maiden_name.is_empty() {
                None
            } else {
                Some(mother_maiden_name.to_string())
            },
            birth_date: birth_date.to_string(),
            sex: sex.to_string(),
        },
    ))
}

fn parse_hl7_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, msh) = parse_msh(input)?;
    let (input, pid) = if input.starts_with("\rPID") {
        let (input, _) = char('\r')(input)?;
        let (input, pid) = parse_pid(input)?;
        (input, Some(pid))
    } else {
        (input, None)
    };

    Ok((input, HL7Message { msh, pid }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <hl7_file>", args[0]);
        std::process::exit(1);
    }

    let contents = fs::read_to_string(&args[1])?;
    match parse_hl7_message(&contents) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Error parsing HL7 message: {:?}", e),
    }

    Ok(())
}