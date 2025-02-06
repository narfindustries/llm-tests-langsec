use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{char, digit1, space0, space1},
    combinator::{map, map_res, opt},
    multi::separated_list0,
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::{env, fs, str::FromStr};

#[derive(Debug)]
struct HttpMessage {
    start_line: StartLine,
    headers: Vec<Header>,
    body: Option<Vec<u8>>,
}

#[derive(Debug)]
enum StartLine {
    Request(RequestLine),
    Response(StatusLine),
}

#[derive(Debug)]
struct RequestLine {
    method: Method,
    uri: String,
    version: HttpVersion,
}

#[derive(Debug)]
struct StatusLine {
    version: HttpVersion,
    status_code: u16,
    reason_phrase: String,
}

#[derive(Debug)]
struct Header {
    name: String,
    value: String,
}

#[derive(Debug)]
enum Method {
    GET,
    POST,
    PUT,
    DELETE,
    HEAD,
    OPTIONS,
    TRACE,
    CONNECT,
}

#[derive(Debug)]
struct HttpVersion {
    major: u8,
    minor: u8,
}

fn parse_method(input: &[u8]) -> IResult<&[u8], Method> {
    alt((
        map(tag("GET"), |_| Method::GET),
        map(tag("POST"), |_| Method::POST),
        map(tag("PUT"), |_| Method::PUT),
        map(tag("DELETE"), |_| Method::DELETE),
        map(tag("HEAD"), |_| Method::HEAD),
        map(tag("OPTIONS"), |_| Method::OPTIONS),
        map(tag("TRACE"), |_| Method::TRACE),
        map(tag("CONNECT"), |_| Method::CONNECT),
    ))(input)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], HttpVersion> {
    let (input, _) = tag("HTTP/")(input)?;
    let (input, major) = map_res(digit1, |s| {
        std::str::from_utf8(s).unwrap().parse::<u8>()
    })(input)?;
    let (input, _) = char('.')(input)?;
    let (input, minor) = map_res(digit1, |s| {
        std::str::from_utf8(s).unwrap().parse::<u8>()
    })(input)?;
    Ok((input, HttpVersion { major, minor }))
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], RequestLine> {
    let (input, method) = parse_method(input)?;
    let (input, _) = space1(input)?;
    let (input, uri) = map(
        take_while1(|c| c != b' '),
        |s: &[u8]| String::from_utf8_lossy(s).into_owned(),
    )(input)?;
    let (input, _) = space1(input)?;
    let (input, version) = parse_version(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((
        input,
        RequestLine {
            method,
            uri,
            version,
        },
    ))
}

fn parse_status_line(input: &[u8]) -> IResult<&[u8], StatusLine> {
    let (input, version) = parse_version(input)?;
    let (input, _) = space1(input)?;
    let (input, status_code) = map_res(digit1, |s| {
        std::str::from_utf8(s).unwrap().parse::<u16>()
    })(input)?;
    let (input, _) = space1(input)?;
    let (input, reason_phrase) = map(
        take_until("\r\n"),
        |s: &[u8]| String::from_utf8_lossy(s).into_owned(),
    )(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((
        input,
        StatusLine {
            version,
            status_code,
            reason_phrase,
        },
    ))
}

fn parse_start_line(input: &[u8]) -> IResult<&[u8], StartLine> {
    alt((
        map(parse_request_line, StartLine::Request),
        map(parse_status_line, StartLine::Response),
    ))(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    let (input, name) = map(
        take_while1(|c| c != b':'),
        |s: &[u8]| String::from_utf8_lossy(s).into_owned(),
    )(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = map(
        take_until("\r\n"),
        |s: &[u8]| String::from_utf8_lossy(s).into_owned(),
    )(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((input, Header { name, value }))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<Header>> {
    let (input, headers) = separated_list0(tag(""), parse_header)(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((input, headers))
}

fn parse_body(input: &[u8]) -> IResult<&[u8], Option<Vec<u8>>> {
    if input.is_empty() {
        Ok((input, None))
    } else {
        Ok((input, Some(input.to_vec())))
    }
}

fn parse_http_message(input: &[u8]) -> IResult<&[u8], HttpMessage> {
    let (input, start_line) = parse_start_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, body) = parse_body(input)?;
    Ok((
        input,
        HttpMessage {
            start_line,
            headers,
            body,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input = fs::read(&args[1]).expect("Failed to read input file");
    match parse_http_message(&input) {
        Ok((remaining, message)) => {
            println!("Parsed message: {:?}", message);
            if !remaining.is_empty() {
                println!("Remaining unparsed data: {:?}", remaining);
            }
        }
        Err(e) => eprintln!("Failed to parse HTTP message: {:?}", e),
    }
}