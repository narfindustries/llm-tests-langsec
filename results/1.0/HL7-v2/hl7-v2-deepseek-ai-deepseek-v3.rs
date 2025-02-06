use nom::{
    bytes::complete::tag,
    character::complete::{alphanumeric1, char, digit1, none_of},
    combinator::{map, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::fs::File;
use std::io::{Read, BufReader};
use std::str;

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

fn parse_msh(input: &[u8]) -> IResult<&[u8], MSH> {
    let (input, _) = tag("MSH")(input)?;
    let (input, (field_separator, encoding_characters, sending_application, sending_facility, receiving_application, receiving_facility, date_time_of_message, message_type, message_control_id, processing_id, version_id)) = tuple((
        char('|'),
        map(many0(none_of("|")), |v| v.iter().collect::<String>()),
        map(many0(none_of("|")), |v| v.iter().collect::<String>()),
        map(many0(none_of("|")), |v| v.iter().collect::<String>()),
        map(many0(none_of("|")), |v| v.iter().collect::<String>()),
        map(many0(none_of("|")), |v| v.iter().collect::<String>()),
        map(many0(none_of("|")), |v| v.iter().collect::<String>()),
        map(many0(none_of("|")), |v| v.iter().collect::<String>()),
        map(many0(none_of("|")), |v| v.iter().collect::<String>()),
        map(many0(none_of("|")), |v| v.iter().collect::<String>()),
        map(many0(none_of("\r\n")), |v| v.iter().collect::<String>()),
    ))(input)?;
    Ok((input, MSH {
        field_separator,
        encoding_characters,
        sending_application,
        sending_facility,
        receiving_application,
        receiving_facility,
        date_time_of_message,
        message_type,
        message_control_id,
        processing_id,
        version_id,
    }))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }
    let filename = &args[1];
    let file = File::open(filename).expect("Failed to open file");
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).expect("Failed to read file");
    let input = str::from_utf8(&buffer).expect("Failed to convert to UTF-8");
    let result = parse_msh(input.as_bytes());
    match result {
        Ok((_, msh)) => println!("Parsed MSH: {:?}", msh),
        Err(e) => eprintln!("Failed to parse MSH: {:?}", e),
    }
}