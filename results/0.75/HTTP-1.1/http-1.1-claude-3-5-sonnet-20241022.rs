use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{char, crlf, digit1, space0, space1},
    combinator::{map, map_res, opt},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::str;
use std::str::FromStr;

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

fn is_token_char(c: char) -> bool {
    let special = "!#$%&'*+-.^_`|~";
    c.is_ascii_alphanumeric() || special.contains(c)
}

fn is_uri_char(c: char) -> bool {
    let special = "!#$%&'*+,-.:;=?@[]_~";
    c.is_ascii_alphanumeric() || special.contains(c)
}

fn parse_token(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(
        take_while1(|c| is_token_char(c as char)),
        str::from_utf8,
    )(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(
        take_while1(|c| is_uri_char(c as char)),
        str::from_utf8,
    )(input)
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    let (input, method) = parse_token(input)?;
    let (input, _) = space1(input)?;
    let (input, uri) = parse_uri(input)?;
    let (input, _) = space1(input)?;
    let (input, version) = preceded(
        tag("HTTP/"),
        map_res(take_while1(|c| c != b'\r'), str::from_utf8),
    )(input)?;
    let (input, _) = crlf(input)?;
    Ok((input, (method.to_string(), uri.to_string(), version.to_string())))
}

fn parse_header_value(input: &[u8]) -> IResult<&[u8], String> {
    let (input, _) = space0(input)?;
    let (input, value) = map_res(
        take_while(|c| c != b'\r' && c != b'\n'),
        str::from_utf8,
    )(input)?;
    let (input, _) = crlf(input)?;
    Ok((input, value.trim().to_string()))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, key) = parse_token(input)?;
    let (input, _) = char(':')(input)?;
    let (input, value) = parse_header_value(input)?;
    Ok((input, (key.to_string(), value)))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], HashMap<String, String>> {
    let (input, headers) = many0(parse_header)(input)?;
    let (input, _) = crlf(input)?;
    Ok((input, headers.into_iter().collect()))
}

fn parse_body(input: &[u8]) -> IResult<&[u8], Option<Vec<u8>>> {
    if input.is_empty() {
        Ok((input, None))
    } else {
        Ok((&input[input.len()..], Some(input.to_vec())))
    }
}

fn parse_http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, uri, version)) = parse_request_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, body) = parse_body(input)?;

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

fn parse_status_line(input: &[u8]) -> IResult<&[u8], (String, u16, String)> {
    let (input, version) = preceded(
        tag("HTTP/"),
        map_res(take_while1(|c| c != b' '), str::from_utf8),
    )(input)?;
    let (input, _) = space1(input)?;
    let (input, status_code) = map_res(digit1, |s: &[u8]| {
        str::from_utf8(s).and_then(|s| u16::from_str(s))
    })(input)?;
    let (input, _) = space1(input)?;
    let (input, status_text) = map_res(
        take_while(|c| c != b'\r'),
        str::from_utf8,
    )(input)?;
    let (input, _) = crlf(input)?;
    Ok((
        input,
        (version.to_string(), status_code, status_text.to_string()),
    ))
}

fn parse_http_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    let (input, (version, status_code, status_text)) = parse_status_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, body) = parse_body(input)?;

    Ok((
        input,
        HttpResponse {
            version,
            status_code,
            status_text,
            headers,
            body,
        },
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input = fs::read(&args[1])?;
    
    // Try to parse as request first, then as response
    match parse_http_request(&input) {
        Ok((_, request)) => println!("Parsed HTTP Request: {:#?}", request),
        Err(_) => match parse_http_response(&input) {
            Ok((_, response)) => println!("Parsed HTTP Response: {:#?}", response),
            Err(e) => eprintln!("Failed to parse HTTP message: {:?}", e),
        },
    }

    Ok(())
}