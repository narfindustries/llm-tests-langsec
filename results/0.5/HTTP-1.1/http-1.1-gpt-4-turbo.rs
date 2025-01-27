use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::complete::{char, digit1, line_ending, space1},
    combinator::{map_res, opt},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::str::{self, FromStr};

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<(String, String)>,
    body: Option<String>,
}

fn parse_method(input: &str) -> IResult<&str, String> {
    map_res(take_until(" "), str::FromStr::from_str)(input)
}

fn parse_uri(input: &str) -> IResult<&str, String> {
    map_res(delimited(space1, take_until(" "), space1), str::FromStr::from_str)(input)
}

fn parse_version(input: &str) -> IResult<&str, String> {
    map_res(take_until("\r\n"), str::FromStr::from_str)(input)
}

fn parse_header(input: &str) -> IResult<&str, (String, String)> {
    let (input, key) = map_res(take_until(":"), str::FromStr::from_str)(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space1(input)?;
    let (input, value) = map_res(take_until("\r\n"), str::FromStr::from_str)(input)?;
    Ok((input, (key, value)))
}

fn parse_headers(input: &str) -> IResult<&str, Vec<(String, String)>> {
    terminated(separated_list0(line_ending, parse_header), pair(line_ending, line_ending))(input)
}

fn parse_body(input: &str) -> IResult<&str, String> {
    map_res(take_while(|_| true), str::FromStr::from_str)(input)
}

fn parse_request(input: &str) -> IResult<&str, HttpRequest> {
    let (input, method) = parse_method(input)?;
    let (input, uri) = parse_uri(input)?;
    let (input, version) = parse_version(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, body) = opt(parse_body)(input)?;

    Ok((
        input,
        HttpRequest {
            method,
            uri,
            version,
            headers,
            body,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Usage: program <path_to_http_request_file>",
        ));
    }

    let mut file = File::open(&args[1])?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;

    match str::from_utf8(&contents) {
        Ok(text) => match parse_request(text) {
            Ok((_, request)) => {
                println!("{:?}", request);
            }
            Err(e) => eprintln!("Failed to parse HTTP request: {:?}", e),
        },
        Err(e) => eprintln!("Invalid UTF-8 sequence: {:?}", e),
    }

    Ok(())
}