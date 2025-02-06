use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{char, none_of},
    multi::many0,
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

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
    patient_name: String,
    birth_date: String,
    sex: char,
    address: Option<String>,
    phone: Option<String>,
    race: Option<String>,
    religion: Option<String>,
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
    if input.starts_with("|") {
        Ok((input, None))
    } else {
        let (input, field) = parse_field(input)?;
        Ok((input, Some(field)))
    }
}

fn parse_msh(input: &str) -> IResult<&str, MSH> {
    let (input, _) = tag("MSH")(input)?;
    let (input, field_separator) = char('|')(input)?;
    let (input, encoding_chars) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, sending_app) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, sending_facility) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, receiving_app) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, receiving_facility) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, datetime) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, security) = parse_optional_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, message_type) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, message_control_id) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, processing_id) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, version_id) = parse_field(input)?;

    Ok((
        input,
        MSH {
            field_separator,
            encoding_chars,
            sending_app,
            sending_facility,
            receiving_app,
            receiving_facility,
            datetime,
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
    let (input, _) = char('|')(input)?;
    let (input, set_id) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_id) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, patient_name) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, birth_date) = parse_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, sex) = none_of("|")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, address) = parse_optional_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, phone) = parse_optional_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, race) = parse_optional_field(input)?;
    let (input, _) = char('|')(input)?;
    let (input, religion) = parse_optional_field(input)?;

    Ok((
        input,
        PID {
            set_id,
            patient_id,
            patient_name,
            birth_date,
            sex,
            address,
            phone,
            race,
            religion,
        },
    ))
}

fn parse_segment(input: &str) -> IResult<&str, String> {
    let (input, segment) = take_until("\r")(input)?;
    Ok((input, segment.to_string()))
}

fn parse_hl7_message(input: &str) -> IResult<&str, HL7Message> {
    let (input, msh) = parse_msh(input)?;
    let (input, _) = char('\r')(input)?;
    
    let (input, pid) = if input.starts_with("PID") {
        let (input, pid) = parse_pid(input)?;
        let (input, _) = char('\r')(input)?;
        (input, Some(pid))
    } else {
        (input, None)
    };

    let (input, segments) = many0(|input| {
        let (input, segment) = parse_segment(input)?;
        let (input, _) = char('\r')(input)?;
        Ok((input, segment))
    })(input)?;

    Ok((input, HL7Message { msh, pid, segments }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input = fs::read_to_string(&args[1]).expect("Failed to read input file");
    match parse_hl7_message(&input) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse HL7 message: {:?}", e),
    }
}