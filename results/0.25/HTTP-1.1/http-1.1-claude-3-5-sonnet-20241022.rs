use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{char, digit1, space0, space1},
    combinator::{map, map_res, opt},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::str;

#[derive(Debug)]
struct HttpMessage {
    start_line: StartLine,
    headers: HashMap<String, String>,
    body: Option<Vec<u8>>,
}

#[derive(Debug)]
enum StartLine {
    Request(RequestLine),
    Response(StatusLine),
}

#[derive(Debug)]
struct RequestLine {
    method: String,
    uri: String,
    version: String,
}

#[derive(Debug)]
struct StatusLine {
    version: String,
    status_code: u16,
    reason_phrase: String,
}

fn is_token_char(c: u8) -> bool {
    match c {
        33..=126 if !b"()<>@,;:\\\"/[]?={} \t".contains(&c) => true,
        _ => false,
    }
}

fn is_header_value_char(c: u8) -> bool {
    match c {
        9 | 32..=126 => true,
        _ => false,
    }
}

fn token(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while1(is_token_char)(input)
}

fn header_value(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while1(is_header_value_char)(input)
}

fn method(input: &[u8]) -> IResult<&[u8], &str> {
    let (input, method) = alt((
        tag("GET"),
        tag("POST"),
        tag("HEAD"),
        tag("PUT"),
        tag("DELETE"),
        tag("CONNECT"),
        tag("OPTIONS"),
        tag("TRACE"),
    ))(input)?;
    Ok((input, str::from_utf8(method).unwrap()))
}

fn http_version(input: &[u8]) -> IResult<&[u8], &str> {
    let (input, version) = tag("HTTP/1.1")(input)?;
    Ok((input, str::from_utf8(version).unwrap()))
}

fn request_line(input: &[u8]) -> IResult<&[u8], RequestLine> {
    let (input, (method, _, uri, _, version, _)) = tuple((
        method,
        space1,
        take_while1(|c| c != b' '),
        space1,
        http_version,
        tag("\r\n"),
    ))(input)?;

    Ok((
        input,
        RequestLine {
            method: method.to_string(),
            uri: String::from_utf8_lossy(uri).into_owned(),
            version: version.to_string(),
        },
    ))
}

fn status_line(input: &[u8]) -> IResult<&[u8], StatusLine> {
    let (input, (version, _, status_code, _, reason, _)) = tuple((
        http_version,
        space1,
        map_res(digit1, |s: &[u8]| {
            str::from_utf8(s).unwrap().parse::<u16>()
        }),
        space1,
        take_until("\r\n"),
        tag("\r\n"),
    ))(input)?;

    Ok((
        input,
        StatusLine {
            version: version.to_string(),
            status_code,
            reason_phrase: String::from_utf8_lossy(reason).into_owned(),
        },
    ))
}

fn header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, (name, _, _, value, _)) = tuple((
        token,
        char(':'),
        space0,
        header_value,
        tag("\r\n"),
    ))(input)?;

    Ok((
        input,
        (
            String::from_utf8_lossy(name).into_owned(),
            String::from_utf8_lossy(value).into_owned(),
        ),
    ))
}

fn headers(input: &[u8]) -> IResult<&[u8], HashMap<String, String>> {
    let (input, headers) = many0(header)(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((input, headers.into_iter().collect()))
}

fn body(input: &[u8]) -> IResult<&[u8], Option<Vec<u8>>> {
    if input.is_empty() {
        Ok((input, None))
    } else {
        Ok((&[], Some(input.to_vec())))
    }
}

fn http_message(input: &[u8]) -> IResult<&[u8], HttpMessage> {
    let (input, start_line) = alt((
        map(request_line, StartLine::Request),
        map(status_line, StartLine::Response),
    ))(input)?;
    let (input, headers) = headers(input)?;
    let (input, body) = body(input)?;

    Ok((
        input,
        HttpMessage {
            start_line,
            headers,
            body,
        },
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input = fs::read(&args[1])?;
    match http_message(&input) {
        Ok((remaining, message)) => {
            println!("Parsed HTTP message: {:#?}", message);
            if !remaining.is_empty() {
                println!("Warning: {} unparsed bytes remaining", remaining.len());
            }
        }
        Err(e) => eprintln!("Failed to parse HTTP message: {}", e),
    }

    Ok(())
}