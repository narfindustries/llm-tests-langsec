use nom::{
    IResult, bytes::complete::{take_while1, tag}, sequence::tuple, combinator::map_res, multi::separated_list1
};
use std::{fs::File, io::Read, env};

fn not_segment_terminator(input: &str) -> IResult<&str, &str> {
    take_while1(|c| c != '\r')(input)
}

fn parse_field(input: &str) -> IResult<&str, &str> {
    take_while1(|c| c != '|')(input)
}

fn parse_component(input: &str) -> IResult<&str, &str> {
    take_while1(|c| c != '^')(input)
}

fn parse_repetition(input: &str) -> IResult<&str, &str> {
    take_while1(|c| c != '~')(input)
}

fn parse_subcomponent(input: &str) -> IResult<&str, &str> {
    take_while1(|c| c != '&')(input)
}

fn parse_field_repetition(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list1(tag("~"), parse_field)(input)
}

fn parse_field_components(input: &str) -> IResult<&str, Vec<Vec<&str>>> {
    separated_list1(tag("|"), map_res(parse_field, |s: &str| {
        separated_list1(tag("^"), map_res(parse_repetition, |r: &str| {
            separated_list1(tag("&"), parse_subcomponent)(r)
        }))(s)
    }))(input)
}

fn parse_segment(input: &str) -> IResult<&str, (&str, Vec<Vec<Vec<&str>>>)> {
    let (input, (segment_id, _, fields)) = tuple((take_while1(|c| c != '|'), tag("|"), parse_field_components))(input)?;
    Ok((input, (segment_id, fields)))
}

fn parse_message(input: &str) -> IResult<&str, Vec<(&str, Vec<Vec<Vec<&str>>>)>> {
    separated_list1(tag("\r"), parse_segment)(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let mut file = match File::open(&args[1]) {
        Ok(file) => file,
        Err(error) => {
            eprintln!("Error opening file: {}", error);
            return;
        },
    };

    let mut contents = String::new();
    if let Err(error) = file.read_to_string(&mut contents) {
        eprintln!("Error reading file: {}", error);
        return;
    }

    match parse_message(&contents) {
        Ok((_, segments)) => {
            for (segment_id, fields) in segments {
                println!("Segment ID: {}", segment_id);
                for (i, field) in fields.iter().enumerate() {
                    println!("  Field {}: {:?}", i+1, field);
                }
            }
        },
        Err(error) => {
            println!("Failed to parse message: {}", error);
        },
    }
}