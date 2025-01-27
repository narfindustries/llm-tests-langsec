use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit1, line_ending, not_line_ending},
    combinator::{map, opt, recognize},
    multi::{many0, many1, separated_list0},
    sequence::{preceded, separated_pair, terminated, tuple},
    IResult,
    error::Error,
    Parser,
};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::str;

#[derive(Debug)]
struct HttpRequest<'a> {
    method: &'a str,
    uri: &'a str,
    version: &'a str,
    headers: HashMap<&'a str, &'a str>,
    body: Option<&'a [u8]>,
}

fn parse_method(input: &[u8]) -> IResult<&[u8], &str> {
    alt((
        map(tag(b"GET"), |_| "GET"),
        map(tag(b"POST"), |_| "POST"),
        map(tag(b"PUT"), |_| "PUT"),
        map(tag(b"DELETE"), |_| "DELETE"),
        map(tag(b"HEAD"), |_| "HEAD"),
        map(tag(b"OPTIONS"), |_| "OPTIONS"),
        map(tag(b"TRACE"), |_| "TRACE"),
        map(tag(b"CONNECT"), |_| "CONNECT"),
        map(tag(b"PATCH"), |_| "PATCH"),
    ))(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], &str> {
    map(take_while1(|c: u8| c != b' '), |uri| str::from_utf8(uri).unwrap())(input)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], &str> {
    map(
        recognize(tuple((
            tag(b"HTTP/"),
            digit1,
            char('.'),
            digit1,
        ))),
        |version| str::from_utf8(version).unwrap(),
    )(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (&str, &str)> {
    separated_pair(
        map(take_while1(|c: u8| c != b':'), |name| str::from_utf8(name).unwrap()),
        tuple((char(':'), take_while(|c: u8| c == b' '))),
        map(not_line_ending, |value| str::from_utf8(value).unwrap())
    )(input)
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], HashMap<&str, &str>> {
    map(
        many0(terminated(parse_header, line_ending)),
        |headers| headers.into_iter().collect()
    )(input)
}

fn parse_body(input: &[u8]) -> IResult<&[u8], Option<&[u8]>> {
    opt(preceded(line_ending, take_while(|_| true)))(input)
}

fn parse_http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    map(
        tuple((
            terminated(parse_method, char(' ')),
            terminated(parse_uri, char(' ')),
            terminated(parse_version, line_ending),
            parse_headers,
            parse_body,
        )),
        |(method, uri, version, headers, body)| HttpRequest {
            method,
            uri,
            version,
            headers,
            body,
        }
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