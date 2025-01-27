use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{char, crlf, digit1, space0, space1},
    combinator::{map, opt, recognize},
    multi::{many0, many1},
    sequence::{preceded, tuple},
    IResult,
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
    map(take_while1(|c: u8| c != b' '), |uri: &[u8]| str::from_utf8(uri).unwrap())(input)
}

fn parse_http_version(input: &[u8]) -> IResult<&[u8], &str> {
    map(recognize(tuple((tag(b"HTTP/"), digit1, char('.'), digit1))), |version: &[u8]| str::from_utf8(version).unwrap())(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (&str, &str)> {
    let (input, key) = take_while1(|c: u8| c != b':')(input)?;
    let (input, _) = tuple((char(':'), space0))(input)?;
    let (input, value) = take_until("\r\n")(input)?;
    let (input, _) = crlf(input)?;
    Ok((input, (str::from_utf8(key).unwrap(), str::from_utf8(value).unwrap())))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], HashMap<&str, &str>> {
    map(many0(parse_header), |headers| headers.into_iter().collect())(input)
}

fn parse_body(input: &[u8]) -> IResult<&[u8], Option<&[u8]>> {
    opt(take_while(|_| true))(input)
}

fn parse_http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, _, uri, _, version, _, _)) = tuple((
        parse_method,
        space1,
        parse_uri,
        space1,
        parse_http_version,
        crlf,
        parse_headers,
    ))(input)?;

    let (input, headers) = parse_headers(input)?;
    let (input, body) = parse_body(input)?;

    Ok((input, HttpRequest {
        method,
        uri,
        version,
        headers,
        body,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input_file = &args[1];
    let input_data = fs::read(input_file).expect("Unable to read file");

    match parse_http_request(&input_data) {
        Ok((_, request)) => {
            println!("Parsed HTTP Request: {:?}", request);
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}