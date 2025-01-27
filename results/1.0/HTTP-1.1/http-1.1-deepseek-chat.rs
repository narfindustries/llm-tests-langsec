use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, crlf, digit1, line_ending, space0, space1},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
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
        take_while1(|c| matches!(c, b'A'..=b'Z' | b'a'..=b'z' | b'-')),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c| c != b' ' && c != b'\r' && c != b'\n'),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], String> {
    map(
        recognize(tuple((tag("HTTP/"), digit1, char('.'), digit1))),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_header_name(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c| matches!(c, b'A'..=b'Z' | b'a'..=b'z' | b'-')),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_header_value(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while(|c| c != b'\r' && c != b'\n'),
        |s: &[u8]| String::from_utf8_lossy(s).trim().to_string(),
    )(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    map(
        separated_pair(
            parse_header_name,
            delimited(space0, char(':'), space0),
            parse_header_value,
        ),
        |(name, value)| Header { name, value },
    )(input)
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<Header>> {
    many0(terminated(parse_header, crlf))(input)
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    tuple((
        terminated(parse_method, space1),
        terminated(parse_uri, space1),
        terminated(parse_version, crlf),
    ))(input)
}

fn parse_body(input: &[u8]) -> IResult<&[u8], Option<Vec<u8>>> {
    if input.is_empty() {
        Ok((input, None))
    } else {
        Ok((&[], Some(input.to_vec())))
    }
}

fn parse_http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    map(
        tuple((
            parse_request_line,
            parse_headers,
            crlf,
            opt(parse_body),
        )),
        |((method, uri, version), headers, _, body)| HttpRequest {
            method,
            uri,
            version,
            headers,
            body,
        },
    )(input)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_http_request(&buffer) {
        Ok((_, request)) => println!("{:#?}", request),
        Err(e) => eprintln!("Failed to parse HTTP request: {:?}", e),
    }

    Ok(())
}