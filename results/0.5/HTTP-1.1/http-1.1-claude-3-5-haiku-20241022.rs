use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{char, digit1, line_ending, not_line_ending, space0, space1},
    combinator::{map, opt, recognize},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult, Parser,
};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::str;

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: HashMap<String, String>,
    body: Option<Vec<u8>>,
}

#[derive(Debug)]
struct HttpResponse {
    version: String,
    status_code: u16,
    status_text: String,
    headers: HashMap<String, String>,
    body: Option<Vec<u8>>,
}

fn parse_method(input: &[u8]) -> IResult<&[u8], String> {
    alt((
        map(tag("GET"), |_| "GET".to_string()),
        map(tag("POST"), |_| "POST".to_string()),
        map(tag("HEAD"), |_| "HEAD".to_string()),
        map(tag("PUT"), |_| "PUT".to_string()),
        map(tag("DELETE"), |_| "DELETE".to_string()),
        map(tag("TRACE"), |_| "TRACE".to_string()),
        map(tag("OPTIONS"), |_| "OPTIONS".to_string()),
        map(tag("CONNECT"), |_| "CONNECT".to_string()),
    ))(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c: u8| c != b' ' && c != b'\r' && c != b'\n'),
        |uri: &[u8]| String::from_utf8_lossy(uri).to_string(),
    )(input)
}

fn parse_http_version(input: &[u8]) -> IResult<&[u8], String> {
    alt((
        map(tag("HTTP/1.1"), |_| "HTTP/1.1".to_string()),
        map(tag("HTTP/1.0"), |_| "HTTP/1.0".to_string()),
    ))(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, key) = take_while1(|c: u8| c != b':')(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = not_line_ending(input)?;
    let (input, _) = line_ending(input)?;

    Ok((
        input,
        (
            String::from_utf8_lossy(key).to_lowercase(),
            String::from_utf8_lossy(value).trim().to_string(),
        ),
    ))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], HashMap<String, String>> {
    map(many0(parse_header), |headers| {
        headers.into_iter().collect::<HashMap<_, _>>()
    })(input)
}

fn parse_http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, method) = parse_method(input)?;
    let (input, _) = space1(input)?;
    let (input, uri) = parse_uri(input)?;
    let (input, _) = space1(input)?;
    let (input, version) = parse_http_version(input)?;
    let (input, _) = line_ending(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, _) = line_ending(input)?;

    let (input, body) = opt(take_while(|_| true))(input)?;

    Ok((input, HttpRequest {
        method,
        uri,
        version,
        headers,
        body: body.map(|b| b.to_vec()),
    }))
}

fn parse_status_code(input: &[u8]) -> IResult<&[u8], u16> {
    map(recognize(take_while1(|c: u8| c >= b'0' && c <= b'9')), |code: &[u8]| {
        str::from_utf8(code).unwrap().parse().unwrap()
    })(input)
}

fn parse_status_text(input: &[u8]) -> IResult<&[u8], String> {
    map(not_line_ending, |text: &[u8]| {
        String::from_utf8_lossy(text).to_string()
    })(input)
}

fn parse_http_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    let (input, version) = parse_http_version(input)?;
    let (input, _) = space1(input)?;
    let (input, status_code) = parse_status_code(input)?;
    let (input, _) = space1(input)?;
    let (input, status_text) = parse_status_text(input)?;
    let (input, _) = line_ending(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, _) = line_ending(input)?;

    let (input, body) = opt(take_while(|_| true))(input)?;

    Ok((input, HttpResponse {
        version,
        status_code,
        status_text,
        headers,
        body: body.map(|b| b.to_vec()),
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input_file = &args[1];
    let input_data = fs::read(input_file).expect("Failed to read input file");

    match parse_http_request(&input_data) {
        Ok((_, request)) => println!("Parsed HTTP Request: {:?}", request),
        Err(_) => match parse_http_response(&input_data) {
            Ok((_, response)) => println!("Parsed HTTP Response: {:?}", response),
            Err(_) => eprintln!("Failed to parse HTTP message"),
        },
    }
}