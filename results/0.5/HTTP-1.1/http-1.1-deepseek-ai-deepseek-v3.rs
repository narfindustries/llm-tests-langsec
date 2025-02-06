use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit1, line_ending, space0, space1},
    combinator::{map, map_res, recognize},
    multi::many0,
    sequence::{pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::{
    collections::HashMap,
    env,
    fs::File,
    io::{self, Read},
};

type HeaderMap = HashMap<String, String>;

fn parse_http_version(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(
        recognize(tuple((tag("HTTP/"), digit1, char('.'), digit1))),
        |s| std::str::from_utf8(s),
    )(input)
}

fn parse_method(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take_while1(|c| c != b' '), |s| std::str::from_utf8(s))(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take_while1(|c| c != b' '), |s| std::str::from_utf8(s))(input)
}

fn parse_header_name(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take_while1(|c| c != b':'), |s| std::str::from_utf8(s))(input)
}

fn parse_header_value(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(
        preceded(space0, take_while(|c| c != b'\r' && c != b'\n')),
        |s| std::str::from_utf8(s),
    )(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (&str, &str)> {
    separated_pair(parse_header_name, char(':'), parse_header_value)(input)
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], HeaderMap> {
    map(
        many0(terminated(parse_header, line_ending)),
        |headers| headers.into_iter().map(|(k, v)| (k.to_lowercase(), v.to_string())).collect(),
    )(input)
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (&str, &str, &str)> {
    tuple((
        parse_method,
        preceded(space1, parse_uri),
        preceded(space1, parse_http_version),
    ))(input)
}

fn parse_response_line(input: &[u8]) -> IResult<&[u8], (&str, &str, &str)> {
    tuple((
        parse_http_version,
        map_res(preceded(space1, digit1), |s| std::str::from_utf8(s)),
        map_res(preceded(space1, take_while1(|c| c != b'\r' && c != b'\n')), |s| std::str::from_utf8(s)),
    ))(input)
}

fn parse_request(input: &[u8]) -> IResult<&[u8], ((&str, &str, &str), HeaderMap)> {
    pair(
        terminated(parse_request_line, line_ending),
        parse_headers,
    )(input)
}

fn parse_response(input: &[u8]) -> IResult<&[u8], ((&str, &str, &str), HeaderMap)> {
    pair(
        terminated(parse_response_line, line_ending),
        parse_headers,
    )(input)
}

fn parse_http_message(input: &[u8]) -> IResult<&[u8], (Option<(&str, &str, &str)>, Option<(&str, &str, &str)>, HeaderMap)> {
    alt((
        map(parse_request, |(req_line, headers)| (Some(req_line), None, headers)),
        map(parse_response, |(resp_line, headers)| (None, Some(resp_line), headers)),
    ))(input)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_http_message(&buffer) {
        Ok((_, (req_line, resp_line, headers))) => {
            if let Some((method, uri, version)) = req_line {
                println!("Request: {} {} {}", method, uri, version);
            }
            if let Some((version, code, reason)) = resp_line {
                println!("Response: {} {} {}", version, code, reason);
            }
            println!("Headers:");
            for (key, value) in headers {
                println!("{}: {}", key, value);
            }
        }
        Err(e) => eprintln!("Failed to parse HTTP message: {:?}", e),
    }

    Ok(())
}