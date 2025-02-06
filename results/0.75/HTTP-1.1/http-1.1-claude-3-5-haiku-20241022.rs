use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit1, multispace0, multispace1, not_line_ending},
    combinator::{map, recognize},
    multi::many0,
    sequence::{delimited, separated_pair, terminated, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<HttpHeader>,
    body: Option<Vec<u8>>,
}

#[derive(Debug)]
struct HttpResponse {
    version: String,
    status_code: u16,
    status_text: String,
    headers: Vec<HttpHeader>,
    body: Option<Vec<u8>>,
}

#[derive(Debug)]
struct HttpHeader {
    name: String,
    value: String,
}

fn is_token_char(c: u8) -> bool {
    matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~')
}

fn parse_method(input: &[u8]) -> IResult<&[u8], String> {
    map(
        alt((
            tag(b"GET"),
            tag(b"POST"),
            tag(b"PUT"),
            tag(b"DELETE"),
            tag(b"HEAD"),
            tag(b"OPTIONS"),
            tag(b"TRACE"),
            tag(b"CONNECT"),
            tag(b"PATCH"),
        )),
        |method| String::from_utf8_lossy(method).into_owned(),
    )(input)
}

fn parse_http_version(input: &[u8]) -> IResult<&[u8], String> {
    map(
        recognize(tuple((
            tag(b"HTTP/"),
            digit1,
            char('.'),
            digit1,
        ))),
        |version| String::from_utf8_lossy(version).into_owned(),
    )(input)
}

fn parse_header_name(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(is_token_char),
        |name| String::from_utf8_lossy(name).into_owned(),
    )(input)
}

fn parse_header_value(input: &[u8]) -> IResult<&[u8], String> {
    map(
        not_line_ending,
        |value| String::from_utf8_lossy(value).into_owned(),
    )(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], HttpHeader> {
    map(
        separated_pair(
            parse_header_name,
            delimited(multispace0, char(':'), multispace0),
            parse_header_value,
        ),
        |(name, value)| HttpHeader { name, value },
    )(input)
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<HttpHeader>> {
    many0(terminated(parse_header, tag(b"\r\n")))(input)
}

fn parse_http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, uri, version)) = tuple((
        terminated(parse_method, char(' ')),
        terminated(take_while(|c| c != b' '), char(' ')),
        terminated(parse_http_version, tag(b"\r\n")),
    ))(input)?;

    let (input, headers) = parse_headers(input)?;
    let (input, _) = tag(b"\r\n")(input)?;

    let body = if !input.is_empty() {
        Some(input.to_vec())
    } else {
        None
    };

    Ok((
        &[],
        HttpRequest {
            method: method.to_string(),
            uri: String::from_utf8_lossy(uri).into_owned(),
            version,
            headers,
            body,
        },
    ))
}

fn parse_status_code(input: &[u8]) -> IResult<&[u8], u16> {
    map(
        take_while1(|c: u8| c >= b'0' && c <= b'9'),
        |code| String::from_utf8_lossy(code).parse().unwrap(),
    )(input)
}

fn parse_http_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    let (input, (version, _, status_code, _, status_text)) = tuple((
        terminated(parse_http_version, char(' ')),
        multispace1,
        terminated(parse_status_code, char(' ')),
        multispace0,
        terminated(not_line_ending, tag(b"\r\n")),
    ))(input)?;

    let (input, headers) = parse_headers(input)?;
    let (input, _) = tag(b"\r\n")(input)?;

    let body = if !input.is_empty() {
        Some(input.to_vec())
    } else {
        None
    };

    Ok((
        &[],
        HttpResponse {
            version,
            status_code,
            status_text: String::from_utf8_lossy(status_text).into_owned(),
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

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_http_request(&buffer) {
        Ok((_, request)) => {
            println!("Parsed HTTP Request: {:?}", request);
        }
        Err(_) => {
            match parse_http_response(&buffer) {
                Ok((_, response)) => {
                    println!("Parsed HTTP Response: {:?}", response);
                }
                Err(e) => {
                    eprintln!("Failed to parse HTTP message: {:?}", e);
                    std::process::exit(1);
                }
            }
        }
    }

    Ok(())
}