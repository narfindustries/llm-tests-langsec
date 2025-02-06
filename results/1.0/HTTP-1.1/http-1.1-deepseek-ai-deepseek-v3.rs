use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::complete::{char, digit1, line_ending, space0, space1},
    combinator::{map, map_res, opt, recognize, value},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::{env, fs, str::from_utf8};

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<Header>,
    body: Option<Vec<u8>>,
}

#[derive(Debug)]
struct HttpResponse {
    version: String,
    status_code: u16,
    reason_phrase: String,
    headers: Vec<Header>,
    body: Option<Vec<u8>>,
}

#[derive(Debug)]
struct Header {
    name: String,
    value: String,
}

fn parse_method(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while(|c| matches!(c, b'A'..=b'Z')),
        |s: &[u8]| from_utf8(s).unwrap().to_string(),
    )(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while(|c| c != b' ' && c != b'\r' && c != b'\n'),
        |s: &[u8]| from_utf8(s).unwrap().to_string(),
    )(input)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], String> {
    map(
        recognize(tuple((tag("HTTP/"), digit1, char('.'), digit1))),
        |s: &[u8]| from_utf8(s).unwrap().to_string(),
    )(input)
}

fn parse_status_code(input: &[u8]) -> IResult<&[u8], u16> {
    map_res(
        take_while(|c| c >= b'0' && c <= b'9'),
        |s: &[u8]| from_utf8(s).unwrap().parse::<u16>(),
    )(input)
}

fn parse_reason_phrase(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while(|c| c != b'\r' && c != b'\n'),
        |s: &[u8]| from_utf8(s).unwrap().to_string(),
    )(input)
}

fn parse_header_name(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while(|c| c != b':' && c != b'\r' && c != b'\n'),
        |s: &[u8]| from_utf8(s).unwrap().to_string(),
    )(input)
}

fn parse_header_value(input: &[u8]) -> IResult<&[u8], String> {
    map(
        preceded(space0, take_while(|c| c != b'\r' && c != b'\n')),
        |s: &[u8]| from_utf8(s).unwrap().to_string(),
    )(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    map(
        separated_pair(parse_header_name, char(':'), parse_header_value),
        |(name, value)| Header { name, value },
    )(input)
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<Header>> {
    many0(terminated(parse_header, line_ending))(input)
}

fn parse_body(input: &[u8]) -> IResult<&[u8], Option<Vec<u8>>> {
    if input.is_empty() {
        Ok((input, None))
    } else {
        Ok((&[], Some(input.to_vec())))
    }
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    tuple((
        parse_method,
        preceded(space1, parse_uri),
        preceded(space1, parse_version),
    ))(input)
}

fn parse_response_line(input: &[u8]) -> IResult<&[u8], (String, u16, String)> {
    tuple((
        parse_version,
        preceded(space1, parse_status_code),
        preceded(space1, parse_reason_phrase),
    ))(input)
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    map(
        tuple((
            terminated(parse_request_line, line_ending),
            parse_headers,
            pair(line_ending, parse_body),
        )),
        |((method, uri, version), headers, (_, body))| HttpRequest {
            method,
            uri,
            version,
            headers,
            body,
        },
    )(input)
}

fn parse_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    map(
        tuple((
            terminated(parse_response_line, line_ending),
            parse_headers,
            pair(line_ending, parse_body),
        )),
        |((version, status_code, reason_phrase), headers, (_, body))| HttpResponse {
            version,
            status_code,
            reason_phrase,
            headers,
            body,
        },
    )(input)
}

fn parse_http(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    parse_request(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    match parse_http(&data) {
        Ok((_, request)) => println!("{:#?}", request),
        Err(e) => eprintln!("Error parsing HTTP request: {:?}", e),
    }
}