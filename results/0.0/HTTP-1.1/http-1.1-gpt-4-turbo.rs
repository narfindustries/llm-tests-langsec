use nom::{
    bytes::complete::{tag, take_until, take_while},
    character::complete::{char, digit1, line_ending, space1},
    combinator::{map_res, opt},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::collections::HashMap;
use std::env;
use std::fs::read_to_string;

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: HashMap<String, String>,
    body: Option<String>,
}

fn parse_request_line(input: &str) -> IResult<&str, (&str, &str, &str)> {
    tuple((
        terminated(take_until(" "), char(' ')),
        terminated(take_until(" "), char(' ')),
        terminated(take_until("\r\n"), line_ending),
    ))(input)
}

fn parse_header(input: &str) -> IResult<&str, (&str, &str)> {
    pair(
        terminated(take_until(":"), char(':')),
        delimited(space1, take_until("\r\n"), line_ending),
    )(input)
}

fn parse_headers(input: &str) -> IResult<&str, HashMap<String, String>> {
    map_res(
        separated_list0(line_ending, parse_header),
        |headers: Vec<(&str, &str)>| {
            let mut header_map = HashMap::new();
            for (key, value) in headers {
                header_map.insert(key.trim().to_string(), value.trim().to_string());
            }
            Ok(header_map)
        },
    )(input)
}

fn parse_http_request(input: &str) -> IResult<&str, HttpRequest> {
    let (input, (method, uri, version)) = parse_request_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, body) = opt(preceded(line_ending, take_until("")))(input)?;

    Ok((
        input,
        HttpRequest {
            method: method.to_string(),
            uri: uri.to_string(),
            version: version.to_string(),
            headers,
            body: body.map(|s| s.to_string()),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let data = read_to_string(filename).expect("Failed to read file");

    match parse_http_request(&data) {
        Ok((_, request)) => println!("{:?}", request),
        Err(e) => eprintln!("Failed to parse HTTP request: {:?}", e),
    }
}