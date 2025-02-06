use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, crlf, digit1, line_ending, one_of, space0},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult, Parser,
};
use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read},
    str::{self, FromStr},
};

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
    reason_phrase: String,
    headers: HashMap<String, String>,
    body: Option<Vec<u8>>,
}

fn parse_method(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c: u8| c.is_ascii_alphabetic()),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c: u8| c != b' ' && c != b'\r' && c != b'\n'),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], String> {
    map(
        recognize(tuple((tag("HTTP/"), digit1, char('.'), digit1))),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, name) = map(
        take_while1(|c: u8| c != b':'),
        |s: &[u8]| String::from_utf8_lossy(s).trim().to_string(),
    )(input)?;
    let (input, _) = preceded(char(':'), space0)(input)?;
    let (input, value) = map(
        take_while(|c: u8| c != b'\r' && c != b'\n'),
        |s: &[u8]| String::from_utf8_lossy(s).trim().to_string(),
    )(input)?;
    Ok((input, (name, value)))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], HashMap<String, String>> {
    map(
        many0(terminated(parse_header, line_ending)),
        |headers| headers.into_iter().collect(),
    )(input)
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    let (input, method) = parse_method(input)?;
    let (input, _) = space0(input)?;
    let (input, uri) = parse_uri(input)?;
    let (input, _) = space0(input)?;
    let (input, version) = parse_version(input)?;
    let (input, _) = line_ending(input)?;
    Ok((input, (method, uri, version)))
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, uri, version)) = parse_request_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, _) = line_ending(input)?;
    let (input, body) = opt(many1(char(' ')).map(|_| input.to_vec()))(input)?;
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
    let (input, version) = parse_version(input)?;
    let (input, _) = space0(input)?;
    let (input, status_code) = map_res(digit1, |s: &[u8]| u16::from_str(str::from_utf8(s).unwrap()))(input)?;
    let (input, _) = space0(input)?;
    let (input, reason_phrase) = map(
        take_while(|c: u8| c != b'\r' && c != b'\n'),
        |s: &[u8]| String::from_utf8_lossy(s).trim().to_string(),
    )(input)?;
    let (input, _) = line_ending(input)?;
    Ok((input, (version, status_code, reason_phrase)))
}

fn parse_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    let (input, (version, status_code, reason_phrase)) = parse_status_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, _) = line_ending(input)?;
    let (input, body) = opt(many1(char(' ')).map(|_| input.to_vec()))(input)?;
    Ok((
        input,
        HttpResponse {
            version,
            status_code,
            reason_phrase,
            headers,
            body,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_request(&buffer) {
        Ok((_, request)) => println!("Parsed Request: {:#?}", request),
        Err(_) => match parse_response(&buffer) {
            Ok((_, response)) => println!("Parsed Response: {:#?}", response),
            Err(e) => eprintln!("Failed to parse: {:?}", e),
        },
    }

    Ok(())
}