extern crate nom;

use nom::{
    bytes::complete::{tag, take_until},
    character::complete::char,
    combinator::map,
    multi::separated_list0,
    sequence::{preceded, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

// Define parsers for different fields using nom
fn parse_field(input: &str) -> IResult<&str, &str> {
    take_until("|")(input)
}

fn parse_msh_segment(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    let (input, _) = tag("MSH")(input)?;
    let (input, _) = char('|')(input)?;
    let fields = separated_list0(char('|'), parse_field);
    map(fields, |f| ("MSH", f))(input)
}

fn parse_pid_segment(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    let (input, _) = tag("PID")(input)?;
    let (input, _) = char('|')(input)?;
    let fields = separated_list0(char('|'), parse_field);
    map(fields, |f| ("PID", f))(input)
}

fn parse_obx_segment(input: &str) -> IResult<&str, (&str, Vec<&str>)> {
    let (input, _) = tag("OBX")(input)?;
    let (input, _) = char('|')(input)?;
    let fields = separated_list0(char('|'), parse_field);
    map(fields, |f| ("OBX", f))(input)
}

// A function to read file content into a string
fn read_file_to_string(filename: &str) -> io::Result<String> {
    let mut file = File::open(filename)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    Ok(content)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    match read_file_to_string(filename) {
        Ok(content) => {
            let (_, msh) = parse_msh_segment(&content).expect("Failed to parse MSH segment");
            println!("Parsed MSH segment: {:?}", msh);

            let (_, pid) = parse_pid_segment(&content).expect("Failed to parse PID segment");
            println!("Parsed PID segment: {:?}", pid);

            let (_, obx) = parse_obx_segment(&content).expect("Failed to parse OBX segment");
            println!("Parsed OBX segment: {:?}", obx);
        },
        Err(e) => eprintln!("Error reading file: {:?}", e),
    }
}