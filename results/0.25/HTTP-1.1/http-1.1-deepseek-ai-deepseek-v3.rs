use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{char, crlf, digit1, space0, space1},
    combinator::{map, map_res, opt},
    multi::{many0, many1},
    sequence::{preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::{
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
struct HttpResponse {
    version: String,
    status_code: String,
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
        alt((
            tag("GET"),
            tag("POST"),
            tag("PUT"),
            tag("DELETE"),
            tag("HEAD"),
            tag("OPTIONS"),
            tag("CONNECT"),
            tag("TRACE"),
            tag("PATCH"),
        )),
        |s| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], String> {
    map(take_until(" "), |s| String::from_utf8_lossy(s).to_string())(input)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], String> {
    map(preceded(tag("HTTP/"), digit1), |s| {
        format!("HTTP/{}", String::from_utf8_lossy(s))
    })(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    map(
        separated_pair(
            map(take_until(":"), |s| String::from_utf8_lossy(s).to_string()),
            tag(":"),
            preceded(
                space0,
                map(take_until("\r\n"), |s| String::from_utf8_lossy(s).to_string()),
            ),
        ),
        |(name, value)| Header { name, value },
    )(input)
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    tuple((
        parse_method,
        preceded(space1, parse_uri),
        preceded(space1, parse_version),
    ))(input)
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    map(
        tuple((
            parse_request_line,
            crlf,
            many0(terminated(parse_header, crlf)),
            opt(crlf),
            opt(many1(char(' '))),
        )),
        |((method, uri, version), _, headers, _, body)| HttpRequest {
            method,
            uri,
            version,
            headers,
            body: body.map(|b| b.into_iter().map(|c| c as u8).collect()),
        },
    )(input)
}

fn parse_status_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    tuple((
        parse_version,
        preceded(space1, map_res(digit1, |s| Ok::<String, nom::error::Error<&[u8]>>(String::from_utf8_lossy(s).to_string()))),
        preceded(
            space1,
            map(take_until("\r\n"), |s| String::from_utf8_lossy(s).to_string()),
        ),
    ))(input)
}

fn parse_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    map(
        tuple((
            parse_status_line,
            crlf,
            many0(terminated(parse_header, crlf)),
            opt(crlf),
            opt(many1(char(' '))),
        )),
        |((version, status_code, reason_phrase), _, headers, _, body)| HttpResponse {
            version,
            status_code,
            reason_phrase,
            headers,
            body: body.map(|b| b.into_iter().map(|c| c as u8).collect()),
        },
    )(input)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let result = parse_request(&buffer).or_else(|_| Err(parse_response(&buffer)));
    match result {
        Ok((_, request)) => println!("{:#?}", request),
        Err(e) => match e {
            nom::Err::Error(_) | nom::Err::Failure(_) => {
                if let Ok((_, response)) = parse_response(&buffer) {
                    println!("{:#?}", response);
                } else {
                    eprintln!("Failed to parse: {:?}", e);
                }
            }
            _ => eprintln!("Failed to parse: {:?}", e),
        },
    }

    Ok(())
}