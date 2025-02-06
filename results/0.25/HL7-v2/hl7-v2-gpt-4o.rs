use nom::{
    bytes::complete::{tag, take_until},
    character::complete::char,
    combinator::opt,
    sequence::tuple,
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
    security: Option<String>,
    message_type: String,
    message_control_id: String,
    processing_id: String,
    version_id: String,
}

fn parse_msh(input: &str) -> IResult<&str, MSH> {
    let (input, (field_separator, encoding_characters, sending_application, sending_facility, receiving_application, receiving_facility, date_time_of_message, security, message_type, message_control_id, processing_id, version_id)) = tuple((
        char('|'),
        take_until("|"),
        take_until("|"),
        take_until("|"),
        take_until("|"),
        take_until("|"),
        take_until("|"),
        opt(take_until("|")),
        take_until("|"),
        take_until("|"),
        take_until("|"),
        take_until("\r"),
    ))(input)?;

    Ok((input, MSH {
        field_separator,
        encoding_characters: encoding_characters.to_string(),
        sending_application: sending_application.to_string(),
        sending_facility: sending_facility.to_string(),
        receiving_application: receiving_application.to_string(),
        receiving_facility: receiving_facility.to_string(),
        date_time_of_message: date_time_of_message.to_string(),
        security: security.map(|s| s.to_string()),
        message_type: message_type.to_string(),
        message_control_id: message_control_id.to_string(),
        processing_id: processing_id.to_string(),
        version_id: version_id.to_string(),
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;

    match parse_msh(&buffer) {
        Ok((_, msh)) => println!("{:?}", msh),
        Err(e) => eprintln!("Failed to parse MSH segment: {:?}", e),
    }

    Ok(())
}