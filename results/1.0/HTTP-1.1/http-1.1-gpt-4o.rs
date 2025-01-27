use nom::{
    bytes::complete::{take_until, take_while, tag},
    character::complete::{space0, space1, digit1},
    combinator::{map_res, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;
use std::str;

#[derive(Debug)]
struct RequestLine {
    method: String,
    request_target: String,
    http_version: String,
}

#[derive(Debug)]
struct Header {
    name: String,
    value: String,
}

#[derive(Debug)]
struct HttpRequest {
    request_line: RequestLine,
    headers: Vec<Header>,
    body: Option<String>,
}

fn parse_method(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take_until(" "), str::from_utf8)(input)
}

fn parse_request_target(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take_until(" "), str::from_utf8)(input)
}

fn parse_http_version(input: &[u8]) -> IResult<&[u8], &str> {
    preceded(tag("HTTP/"), map_res(take_while(|c| c.is_ascii_digit() || c == b'.'), str::from_utf8))(input)
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], RequestLine> {
    let (input, (method, _, request_target, _, http_version, _)) = tuple((
        parse_method,
        space1,
        parse_request_target,
        space1,
        parse_http_version,
        tag("\r\n"),
    ))(input)?;
    Ok((input, RequestLine { method: method.to_string(), request_target: request_target.to_string(), http_version: http_version.to_string() }))
}

fn parse_header_name(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take_until(":"), str::from_utf8)(input)
}

fn parse_header_value(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(preceded(tag(": "), take_until("\r\n")), str::from_utf8)(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    let (input, (name, value, _)) = tuple((
        parse_header_name,
        parse_header_value,
        tag("\r\n"),
    ))(input)?;
    Ok((input, Header { name: name.to_string(), value: value.to_string() }))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<Header>> {
    separated_list0(tag("\r\n"), parse_header)(input)
}

fn parse_http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, request_line) = parse_request_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, _) = tag("\r\n")(input)?;
    let (input, body) = opt(map_res(take_until(""), str::from_utf8))(input)?;
    Ok((input, HttpRequest { request_line, headers, body: body.map(|s| s.to_string()) }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_http_request(&buffer) {
        Ok((_, request)) => println!("{:#?}", request),
        Err(e) => eprintln!("Failed to parse HTTP request: {:?}", e),
    }

    Ok(())
}