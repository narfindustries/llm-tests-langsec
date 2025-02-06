use nom::{
    bytes::complete::{tag, take_until},
    character::complete::alphanumeric1,
    combinator::opt,
    multi::separated_list0,
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

// Define data structures for different segment types
#[derive(Debug)]
struct MSH {
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
    patient_id: String,
    patient_name: String,
    date_of_birth: String,
    sex: String,
    patient_address: String,
    phone_number_home: String,
}

#[derive(Debug)]
struct ORC {
    order_control: String,
    placer_order_number: String,
    filler_order_number: String,
    order_status: String,
    quantity_timing: String,
}

// Define parsers for each field
fn parse_field(input: &str) -> IResult<&str, &str> {
    take_until("|")(input)
}

fn parse_msh(input: &str) -> IResult<&str, MSH> {
    let (input, _) = tag("MSH")(input)?;
    let (input, _) = tag("|")(input)?;
    let (input, fields) = separated_list0(tag("|"), parse_field)(input)?;

    Ok((
        input,
        MSH {
            sending_application: fields[2].to_string(),
            sending_facility: fields[3].to_string(),
            receiving_application: fields[4].to_string(),
            receiving_facility: fields[5].to_string(),
            date_time_of_message: fields[6].to_string(),
            message_type: fields[8].to_string(),
            message_control_id: fields[9].to_string(),
            processing_id: fields[10].to_string(),
            version_id: fields[11].to_string(),
        },
    ))
}

fn parse_pid(input: &str) -> IResult<&str, PID> {
    let (input, _) = tag("PID")(input)?;
    let (input, _) = tag("|")(input)?;
    let (input, fields) = separated_list0(tag("|"), parse_field)(input)?;

    Ok((
        input,
        PID {
            patient_id: fields[2].to_string(),
            patient_name: fields[4].to_string(),
            date_of_birth: fields[6].to_string(),
            sex: fields[7].to_string(),
            patient_address: fields[10].to_string(),
            phone_number_home: fields[12].to_string(),
        },
    ))
}

fn parse_orc(input: &str) -> IResult<&str, ORC> {
    let (input, _) = tag("ORC")(input)?;
    let (input, _) = tag("|")(input)?;
    let (input, fields) = separated_list0(tag("|"), parse_field)(input)?;

    Ok((
        input,
        ORC {
            order_control: fields[0].to_string(),
            placer_order_number: fields[1].to_string(),
            filler_order_number: fields[2].to_string(),
            order_status: fields[4].to_string(),
            quantity_timing: fields[6].to_string(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "Invalid number of arguments"));
    }

    let mut file = File::open(&args[1])?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    // Parse the HL7 message
    let (_, msh) = parse_msh(&contents).unwrap();
    println!("{:?}", msh);
    let (_, pid) = parse_pid(&contents).unwrap();
    println!("{:?}", pid);
    let (_, orc) = parse_orc(&contents).unwrap();
    println!("{:?}", orc);

    Ok(())
}