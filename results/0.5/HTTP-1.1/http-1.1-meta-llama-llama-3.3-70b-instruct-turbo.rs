use nom::{
    bytes::complete::{tag, take_till, take_while},
    character::complete::{char, digit1, multispace0, multispace1, newline},
    combinator::{map, map_res, opt, recognize},
    IResult,
};
use std::{
    env, fs,
    io::{self, Read},
    str,
};

#[derive(Debug, PartialEq)]
enum HttpMethod {
    GET,
    POST,
    PUT,
    DELETE,
    HEAD,
    OPTIONS,
    CONNECT,
    PATCH,
}

fn method(input: &str) -> IResult<&str, HttpMethod> {
    let (input, method) = take_while(|c| c.is_alphabetic())(input)?;
    match method.to_uppercase().as_str() {
        "GET" => Ok((input, HttpMethod::GET)),
        "POST" => Ok((input, HttpMethod::POST)),
        "PUT" => Ok((input, HttpMethod::PUT)),
        "DELETE" => Ok((input, HttpMethod::DELETE)),
        "HEAD" => Ok((input, HttpMethod::HEAD)),
        "OPTIONS" => Ok((input, HttpMethod::OPTIONS)),
        "CONNECT" => Ok((input, HttpMethod::CONNECT)),
        "PATCH" => Ok((input, HttpMethod::PATCH)),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    }
}

#[derive(Debug, PartialEq)]
enum HttpVersion {
    HTTP1_0,
    HTTP1_1,
}

fn version(input: &str) -> IResult<&str, HttpVersion> {
    let (input, _) = tag("HTTP/")(input)?;
    let (input, major) = map_res(digit1, str::parse::<u8>)(input)?;
    let (input, _) = char('.')(input)?;
    let (input, minor) = map_res(digit1, str::parse::<u8>)(input)?;
    match (major, minor) {
        (1, 0) => Ok((input, HttpVersion::HTTP1_0)),
        (1, 1) => Ok((input, HttpVersion::HTTP1_1)),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::Digit)),
    }
}

#[derive(Debug, PartialEq)]
struct HttpStatus {
    code: u16,
    reason: String,
}

fn status(input: &str) -> IResult<&str, HttpStatus> {
    let (input, code) = map_res(take_while(|c| c.is_digit(10)), str::parse::<u16>)(input)?;
    let (input, _) = multispace1(input)?;
    let (input, reason) = take_till(|c| c == '\r' || c == '\n')(input)?;
    let (input, _) = newline(input)?;
    Ok((input, HttpStatus { code, reason: reason.to_string() }))
}

#[derive(Debug, PartialEq)]
struct HttpMessage {
    method: Option<HttpMethod>,
    version: Option<HttpVersion>,
    status: Option<HttpStatus>,
    headers: Vec<(&str, &str)>,
    body: Option<String>,
}

fn header(input: &str) -> IResult<&str, (&str, &str)> {
    let (input, name) = take_while(|c| c.is_alphanumeric() || c == '-' || c == '_')(input)?;
    let (input, _) = tag(": ")(input)?;
    let (input, value) = take_till(|c| c == '\r' || c == '\n')(input)?;
    let (input, _) = newline(input)?;
    Ok((input, (name, value)))
}

fn message(input: &str) -> IResult<&str, HttpMessage> {
    let (input, method) = opt(method)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, version) = opt(version)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, status) = opt(status)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, headers) = recognize(multispace0)(input)?;
    let (input, headers) = if headers.is_empty() {
        (input, Vec::new())
    } else {
        let (input, headers) = recognize(multispace0)(input)?;
        let (input, headers) = headers
            .split("\r\n")
            .map(|h| header(h).unwrap().1)
            .collect::<Vec<(&str, &str)>>();

        (input, headers)
    };
    let (input, body) = if input.is_empty() {
        (input, None)
    } else {
        let (input, body) = recognize(take_till(|c| c == '\0'))(input)?;
        (input, Some(body.to_string()))
    };
    Ok((
        input,
        HttpMessage {
            method,
            version,
            status,
            headers,
            body,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <file>", args[0]);
    }
    let mut file = fs::File::open(&args[1])?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;
    let input = str::from_utf8(&contents).unwrap();
    let (_, msg) = message(input).unwrap();
    println!("{:?}", msg);
    Ok(())
}