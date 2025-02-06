use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, digit1, multispace0},
    combinator::{map, opt},
    multi::{many0},
    sequence::{terminated, tuple},
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

fn parse_method(input: &[u8]) -> IResult<&[u8], String> {
    alt((
        map(tag(b"GET"), |_| "GET".to_string()),
        map(tag(b"POST"), |_| "POST".to_string()),
        map(tag(b"PUT"), |_| "PUT".to_string()),
        map(tag(b"DELETE"), |_| "DELETE".to_string()),
        map(tag(b"HEAD"), |_| "HEAD".to_string()),
        map(tag(b"OPTIONS"), |_| "OPTIONS".to_string()),
        map(tag(b"TRACE"), |_| "TRACE".to_string()),
        map(tag(b"CONNECT"), |_| "CONNECT".to_string()),
    ))(input)
}

fn parse_http_version(input: &[u8]) -> IResult<&[u8], String> {
    map(
        tuple((
            tag(b"HTTP/"),
            take_while1(|c: u8| c == b'.' || (c >= b'0' && c <= b'9')),
        )),
        |(_, version)| String::from_utf8_lossy(version).to_string(),
    )(input)
}

fn parse_header_name(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c: u8| c != b':' && c != b'\r' && c != b'\n'),
        |name| String::from_utf8_lossy(name).to_string(),
    )(input)
}

fn parse_header_value(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c: u8| c != b'\r' && c != b'\n'),
        |value| String::from_utf8_lossy(value).to_string(),
    )(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], HttpHeader> {
    map(
        tuple((
            terminated(parse_header_name, char(':')),
            multispace0,
            terminated(parse_header_value, tag(b"\r\n")),
        )),
        |(name, _, value)| HttpHeader { name, value },
    )(input)
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    map(
        tuple((
            terminated(parse_method, char(' ')),
            terminated(take_while1(|c: u8| c != b' '), char(' ')),
            terminated(parse_http_version, tag(b"\r\n")),
            many0(parse_header),
            opt(take_while1(|_| true)),
        )),
        |(method, uri, version, headers, body)| HttpRequest {
            method: method.to_string(),
            uri: String::from_utf8_lossy(uri).to_string(),
            version,
            headers,
            body: body.map(|b| b.to_vec()),
        },
    )(input)
}

fn parse_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    map(
        tuple((
            terminated(parse_http_version, char(' ')),
            terminated(map(digit1, |s: &[u8]| std::str::from_utf8(s).unwrap().parse::<u16>().unwrap()), char(' ')),
            terminated(take_while1(|c: u8| c != b'\r' && c != b'\n'), tag(b"\r\n")),
            many0(parse_header),
            opt(take_while1(|_| true)),
        )),
        |(version, status_code, status_text, headers, body)| HttpResponse {
            version,
            status_code,
            status_text: String::from_utf8_lossy(status_text).to_string(),
            headers,
            body: body.map(|b| b.to_vec()),
        },
    )(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_request(&buffer) {
        Ok((_, request)) => {
            println!("Parsed Request: {:?}", request);
        }
        Err(_) => {
            match parse_response(&buffer) {
                Ok((_, response)) => {
                    println!("Parsed Response: {:?}", response);
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