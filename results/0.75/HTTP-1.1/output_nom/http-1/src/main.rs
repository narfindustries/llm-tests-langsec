use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_till1},
    character::complete::{char, digit1, multispace0, multispace1},
    combinator::{map, map_res, opt, recognize},
    error::{context, ErrorKind, ParseError},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::{env, fs, io, str};

#[derive(Debug, PartialEq)]
enum HttpMethod {
    Get,
    Head,
    Post,
    Put,
    Delete,
    Connect,
    Options,
    Trace,
}

fn http_method(input: &str) -> IResult<&str, HttpMethod> {
    alt((
        map(tag("GET"), |_| HttpMethod::Get),
        map(tag("HEAD"), |_| HttpMethod::Head),
        map(tag("POST"), |_| HttpMethod::Post),
        map(tag("PUT"), |_| HttpMethod::Put),
        map(tag("DELETE"), |_| HttpMethod::Delete),
        map(tag("CONNECT"), |_| HttpMethod::Connect),
        map(tag("OPTIONS"), |_| HttpMethod::Options),
        map(tag("TRACE"), |_| HttpMethod::Trace),
    ))(input)
}

#[derive(Debug, PartialEq)]
enum HttpVersion {
    Http1_0,
    Http1_1,
}

fn http_version(input: &str) -> IResult<&str, HttpVersion> {
    alt((
        map(tag("HTTP/1.0"), |_| HttpVersion::Http1_0),
        map(tag("HTTP/1.1"), |_| HttpVersion::Http1_1),
    ))(input)
}

#[derive(Debug, PartialEq)]
struct HttpResponseStatusLine {
    version: HttpVersion,
    status_code: u16,
    reason_phrase: String,
}

fn http_response_status_line(input: &str) -> IResult<&str, HttpResponseStatusLine> {
    map(
        tuple((http_version, char(' '), digit1, char(' '), take_till1(|c| c == '\r' || c == '\n'))),
        |(version, _, status_code, _, reason_phrase)| HttpResponseStatusLine {
            version,
            status_code: status_code.parse().unwrap(),
            reason_phrase: reason_phrase.to_string(),
        },
    )(input)
}

#[derive(Debug, PartialEq)]
struct HttpRequestStatusLine {
    method: HttpMethod,
    request_target: String,
    version: HttpVersion,
}

fn http_request_status_line(input: &str) -> IResult<&str, HttpRequestStatusLine> {
    map(
        tuple((http_method, char(' '), take_till1(|c| c == ' '), char(' '), http_version)),
        |(method, _, request_target, _, version)| HttpRequestStatusLine {
            method,
            request_target: request_target.to_string(),
            version,
        },
    )(input)
}

#[derive(Debug, PartialEq)]
struct HeaderField {
    name: String,
    value: String,
}

fn header_field(input: &str) -> IResult<&str, HeaderField> {
    map(
        tuple((take_till1(|c| c == ':'), char(':'), multispace0, take_till1(|c| c == '\r'))),
        |(name, _, _, value)| HeaderField {
            name: name.to_string(),
            value: value.to_string(),
        },
    )(input)
}

#[derive(Debug, PartialEq)]
struct HttpMessage {
    start_line: HttpRequestStatusLine,
    headers: Vec<HeaderField>,
    body: Vec<u8>,
}

fn crlf(input: &str) -> IResult<&str, &str> {
    tag("\r\n")(input)
}

fn http_message(input: &str) -> IResult<&str, HttpMessage> {
    map(
        tuple((
            http_request_status_line,
            many0(preceded(crlf, header_field)),
            crlf,
            recognize(many0(nom::character::complete::any_u8)),
        )),
        |(start_line, headers, _, body)| HttpMessage {
            start_line,
            headers,
            body: body.into_bytes(),
        },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let input_file = &args[1];
    let input = fs::read(input_file).expect("Failed to read input file");
    let input_str = str::from_utf8(&input).expect("Failed to convert input to string");

    match http_message(input_str) {
        Ok((remaining, message)) => println!("{:?}", message),
        Err(err) => eprintln!("Error parsing HTTP message: {:?}", err),
    }
}