use nom::{
    bytes::complete::tag,
    character::complete::{alphanumeric1, char},
    combinator::{map_res, opt},
    multi::separated_list0,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct MSHSegment {
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

fn parse_field(input: &str) -> IResult<&str, String> {
    map_res(alphanumeric1, |s: &str| Ok::<String, std::convert::Infallible>(s.to_string()))(input)
}

fn parse_optional_field(input: &str) -> IResult<&str, Option<String>> {
    opt(map_res(alphanumeric1, |s: &str| Ok::<String, std::convert::Infallible>(s.to_string())))(input)
}

fn parse_msh_segment(input: &str) -> IResult<&str, MSHSegment> {
    let (input, _) = tag("MSH")(input)?;
    let (input, _) = char('|')(input)?;
    let (input, fields) = separated_list0(char('|'), parse_field)(input)?;

    let msh_segment = MSHSegment {
        sending_application: fields[2].clone(),
        sending_facility: fields[3].clone(),
        receiving_application: fields[4].clone(),
        receiving_facility: fields[5].clone(),
        date_time_of_message: fields[6].clone(),
        security: fields.get(7).cloned(),
        message_type: fields[8].clone(),
        message_control_id: fields[9].clone(),
        processing_id: fields[10].clone(),
        version_id: fields[11].clone(),
    };

    Ok((input, msh_segment))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No input file specified"));
    }

    let file_path = &args[1];
    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match parse_msh_segment(&contents) {
        Ok((_, msh)) => {
            println!("{:?}", msh);
        }
        Err(e) => {
            eprintln!("Failed to parse MSH segment: {:?}", e);
        }
    }

    Ok(())
}