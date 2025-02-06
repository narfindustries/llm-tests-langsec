use nom::{
    bytes::complete::{tag, take_while},
    character::complete::{char, digit1, space0, space1},
    combinator::{map_res, opt},
    multi::separated_list0,
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::str::FromStr;

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<(String, String)>,
}

fn is_token(c: char) -> bool {
    c.is_alphanumeric() || "-!#$%&'*+.^_`|~".contains(c)
}

fn is_header_value(c: char) -> bool {
    c.is_ascii_graphic() || c == ' '
}

fn parse_method(input: &str) -> IResult<&str, &str> {
    take_while(is_token)(input)
}

fn parse_uri(input: &str) -> IResult<&str, &str> {
    take_while(|c| c != ' ')(input)
}

fn parse_version(input: &str) -> IResult<&str, &str> {
    preceded(tag("HTTP/"), take_while(|c: char| c.is_digit(10) || c == '.'))(input)
}

fn parse_header(input: &str) -> IResult<&str, (String, String)> {
    let (input, name) = take_while(is_token)(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = take_while(is_header_value)(input)?;
    Ok((input, (name.to_string(), value.trim().to_string())))
}

fn parse_request(input: &str) -> IResult<&str, HttpRequest> {
    let (input, (method, _, uri, _, version, _)) = tuple((
        parse_method,
        space1,
        parse_uri,
        space1,
        parse_version,
        char('\n'),
    ))(input)?;

    let (input, headers) = separated_list0(char('\n'), parse_header)(input)?;

    Ok((
        input,
        HttpRequest {
            method: method.to_string(),
            uri: uri.to_string(),
            version: version.to_string(),
            headers,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;

    match parse_request(&buffer) {
        Ok((_, request)) => println!("{:?}", request),
        Err(e) => eprintln!("Failed to parse HTTP request: {:?}", e),
    }

    Ok(())
}