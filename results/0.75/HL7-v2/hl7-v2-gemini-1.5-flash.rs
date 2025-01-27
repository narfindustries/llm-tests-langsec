use nom::{
    bytes::complete::{tag, take_while, take_until},
    character::complete::{alphanumeric1, char, line_ending},
    combinator::{map, map_res, opt, recognize},
    multi::many0,
    sequence::{delimited, separated_pair},
    IResult,
};
use std::env;
use std::fs;
use std::path::Path;

#[derive(Debug, PartialEq)]
struct Hl7Message {
    segments: Vec<Hl7Segment>,
}

#[derive(Debug, PartialEq)]
struct Hl7Segment {
    field_separator: char,
    component_separator: char,
    repetition_separator: char,
    escape_character: char,
    subcomponent_separator: char,
    fields: Vec<Hl7Field>,
}

#[derive(Debug, PartialEq)]
struct Hl7Field {
    components: Vec<Hl7Component>,
}


#[derive(Debug, PartialEq)]
struct Hl7Component {
    subcomponents: Vec<String>,
}


fn hl7_message(input: &[u8]) -> IResult<&[u8], Hl7Message> {
    let (input, segments) = many0(hl7_segment)(input)?;
    Ok((input, Hl7Message { segments }))
}

fn hl7_segment(input: &[u8]) -> IResult<&[u8], Hl7Segment> {
    let (input, msh_segment) =  map(recognize(many0(take_until("\r"))), |bytes| {
            let segment_str = std::str::from_utf8(bytes).unwrap();
            let parts:Vec<&str> = segment_str.split('|').collect();
            if parts.len() > 0 {
                let field_separator = '|';
                let component_separator = '^';
                let repetition_separator = '~';
                let escape_character = '\\';
                let subcomponent_separator = '&';
                let mut fields = Vec::new();
                for i in 1..parts.len() {
                    let components = parts[i].split('^').map(|s| s.to_string()).collect();
                    fields.push(Hl7Field{components});
                }
                Hl7Segment { field_separator, component_separator, repetition_separator, escape_character, subcomponent_separator, fields }
            } else {
                Hl7Segment { field_separator: '|', component_separator: '^', repetition_separator: '~', escape_character: '\\', subcomponent_separator: '&', fields: Vec::new() }
            }
        })(input)?;
    let (input, _) = line_ending(input)?;
    Ok((input, msh_segment))
}

fn hl7_field(input: &[u8]) -> IResult<&[u8], Hl7Field> {
    let (input, components) = many0(hl7_component)(input)?;
    Ok((input, Hl7Field { components }))
}

fn hl7_component(input: &[u8]) -> IResult<&[u8], Hl7Component> {
    let (input, subcomponents) = many0(map(take_while(|c| c != '&' as u8), |bytes| {
        std::str::from_utf8(bytes).unwrap().to_string()
    }))(input)?;
    Ok((input, Hl7Component { subcomponents }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let file_content = fs::read(path).expect("Failed to read file");
    let result = hl7_message(&file_content);

    match result {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => println!("Error parsing HL7 message: {:?}", e),
    }
}
