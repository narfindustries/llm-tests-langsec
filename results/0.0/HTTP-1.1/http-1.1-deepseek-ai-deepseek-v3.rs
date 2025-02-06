use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_until, take_while1},
    character::complete::{char, digit1, space0, space1},
    combinator::{map, map_res},
    multi::many0,
    sequence::{delimited, preceded},
    IResult,
};
use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read},
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
    map(
        delimited(tag("HTTP/"), take_while1(|c| c != b'\r'), tag("\r\n")),
        |s| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, key) = map(
        take_while1(|c| c != b':'),
        |s| String::from_utf8_lossy(s).to_string(),
    )(input)?;
    let (input, _) = preceded(char(':'), space0)(input)?;
    let (input, value) = map(
        take_until("\r\n"),
        |s| String::from_utf8_lossy(s).to_string(),
    )(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((input, (key, value)))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], HashMap<String, String>> {
    map(many0(parse_header), |headers| {
        headers.into_iter().collect()
    })(input)
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    let (input, method) = parse_method(input)?;
    let (input, _) = space1(input)?;
    let (input, uri) = parse_uri(input)?;
    let (input, _) = space1(input)?;
    let (input, version) = parse_version(input)?;
    Ok((input, (method, uri, version)))
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, uri, version)) = parse_request_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, body) = if headers.contains_key("Content-Length") {
        let length: usize = headers["Content-Length"].parse().unwrap();
        map(take(length), |s: &[u8]| Some(s.to_vec()))(input)?
    } else {
        (input, None)
    };
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
    let (input, _) = space1(input)?;
    let (input, status_code) = map_res(digit1, |s: &[u8]| {
        String::from_utf8_lossy(s).parse::<u16>()
    })(input)?;
    let (input, _) = space1(input)?;
    let (input, reason_phrase) = map(take_until("\r\n"), |s| {
        String::from_utf8_lossy(s).to_string()
    })(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((input, (version, status_code, reason_phrase)))
}

fn parse_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    let (input, (version, status_code, reason_phrase)) = parse_status_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, body) = if headers.contains_key("Content-Length") {
        let length: usize = headers["Content-Length"].parse().unwrap();
        map(take(length), |s: &[u8]| Some(s.to_vec()))(input)?
    } else {
        (input, None)
    };
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

    let result = alt((map(parse_request, |r| Either::Left(r)), map(parse_response, |r| Either::Right(r))))(&buffer);
    match result {
        Ok((_, Either::Left(request))) => println!("Request: {:?}", request),
        Ok((_, Either::Right(response))) => println!("Response: {:?}", response),
        Err(e) => eprintln!("Failed to parse: {:?}", e),
    }

    Ok(())
}

enum Either<L, R> {
    Left(L),
    Right(R),
}