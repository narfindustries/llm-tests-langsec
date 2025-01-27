use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, crlf, digit1, space0, space1},
    combinator::{map, opt, recognize},
    multi::{many0, many1},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs;
use std::str;

#[derive(Debug)]
struct HttpRequest<'a> {
    method: &'a str,
    uri: &'a str,
    version: &'a str,
    headers: Vec<(&'a str, &'a str)>,
    body: Option<&'a [u8]>,
}

fn is_token_char(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' | '.' | '~')
}

fn parse_method(input: &[u8]) -> IResult<&[u8], &str> {
    map(
        recognize(take_while1(|c| is_token_char(c as char))),
        |m| str::from_utf8(m).unwrap(),
    )(input)
}

fn parse_request_uri(input: &[u8]) -> IResult<&[u8], &str> {
    map(
        recognize(take_while(|c| c != b' ' && c != b'\r' && c != b'\n')),
        |uri| str::from_utf8(uri).unwrap(),
    )(input)
}

fn parse_http_version(input: &[u8]) -> IResult<&[u8], &str> {
    map(
        recognize(tuple((tag(b"HTTP/"), digit1, tag(b"."), digit1))),
        |v| str::from_utf8(v).unwrap(),
    )(input)
}

fn parse_header_name(input: &[u8]) -> IResult<&[u8], &str> {
    map(
        recognize(take_while1(|c| is_token_char(c as char) && c != b':' )),
        |name| str::from_utf8(name).unwrap(),
    )(input)
}

fn parse_header_value(input: &[u8]) -> IResult<&[u8], &str> {
    map(
        recognize(take_while(|c| c != b'\r' && c != b'\n')),
        |value| str::from_utf8(value).unwrap(),
    )(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (&str, &str)> {
    map(
        tuple((
            parse_header_name,
            preceded(char(':'), space0),
            parse_header_value,
            crlf,
        )),
        |(name, _, value, _)| (name, value),
    )(input)
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<(&str, &str)>> {
    many0(parse_header)(input)
}

fn parse_body(input: &[u8]) -> IResult<&[u8], Option<&[u8]>> {
    opt(take_while(|_| true))(input)
}

fn parse_http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    map(
        tuple((
            parse_method,
            space1,
            parse_request_uri,
            space1,
            parse_http_version,
            crlf,
            parse_headers,
            crlf,
            parse_body,
        )),
        |(method, _, uri, _, version, _, headers, _, body)| HttpRequest {
            method,
            uri,
            version,
            headers,
            body,
        },
    )(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input = fs::read(&args[1])?;

    match parse_http_request(&input) {
        Ok((_, request)) => {
            println!("Parsed HTTP Request: {:?}", request);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}