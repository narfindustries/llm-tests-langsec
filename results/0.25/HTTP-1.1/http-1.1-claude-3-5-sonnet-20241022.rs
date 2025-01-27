use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{char, digit1, space0, space1},
    combinator::{map, map_res, opt},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::{collections::HashMap, env, fs, str};

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
    status_text: String,
    headers: HashMap<String, String>,
    body: Option<Vec<u8>>,
}

fn is_token_char(c: u8) -> bool {
    matches!(c, 33..=126 if !b"()<>@,;:\\\"/[]?={} \t".contains(&c))
}

fn is_header_value_char(c: u8) -> bool {
    matches!(c, 32..=126 | 9)
}

fn method(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(
        take_while1(is_token_char),
        |method: &[u8]| str::from_utf8(method),
    )(input)
}

fn uri(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(
        take_while1(|c| c != b' '),
        |uri: &[u8]| str::from_utf8(uri),
    )(input)
}

fn http_version(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(
        take_while1(|c| c != b'\r'),
        |version: &[u8]| str::from_utf8(version),
    )(input)
}

fn header_name(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(
        take_while1(is_token_char),
        |name: &[u8]| str::from_utf8(name),
    )(input)
}

fn header_value(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(
        take_while1(is_header_value_char),
        |value: &[u8]| str::from_utf8(value),
    )(input)
}

fn header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, name) = terminated(header_name, tag(": "))(input)?;
    let (input, value) = terminated(header_value, tag("\r\n"))(input)?;
    Ok((input, (name.to_string(), value.trim().to_string())))
}

fn headers(input: &[u8]) -> IResult<&[u8], HashMap<String, String>> {
    let (input, headers) = many0(header)(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((input, headers.into_iter().collect()))
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, method) = terminated(method, space1)(input)?;
    let (input, uri) = terminated(uri, space1)(input)?;
    let (input, version) = terminated(http_version, tag("\r\n"))(input)?;
    let (input, headers) = headers(input)?;
    let (input, body) = opt(rest)(input)?;

    Ok((
        input,
        HttpRequest {
            method: method.to_string(),
            uri: uri.to_string(),
            version: version.to_string(),
            headers,
            body: body.map(|b| b.to_vec()),
        },
    ))
}

fn status_code(input: &[u8]) -> IResult<&[u8], u16> {
    map_res(digit1, |code: &[u8]| {
        str::from_utf8(code).unwrap().parse::<u16>()
    })(input)
}

fn status_text(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(
        take_while1(|c| c != b'\r'),
        |text: &[u8]| str::from_utf8(text),
    )(input)
}

fn parse_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    let (input, version) = terminated(http_version, space1)(input)?;
    let (input, status_code) = terminated(status_code, space1)(input)?;
    let (input, status_text) = terminated(status_text, tag("\r\n"))(input)?;
    let (input, headers) = headers(input)?;
    let (input, body) = opt(rest)(input)?;

    Ok((
        input,
        HttpResponse {
            version: version.to_string(),
            status_code,
            status_text: status_text.to_string(),
            headers,
            body: body.map(|b| b.to_vec()),
        },
    ))
}

fn rest(input: &[u8]) -> IResult<&[u8], &[u8]> {
    Ok((&[], input))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input = fs::read(&args[1])?;
    
    // Try parsing as request first, then response
    match parse_request(&input) {
        Ok((_, request)) => println!("Parsed HTTP Request: {:#?}", request),
        Err(_) => match parse_response(&input) {
            Ok((_, response)) => println!("Parsed HTTP Response: {:#?}", response),
            Err(e) => eprintln!("Failed to parse HTTP message: {:?}", e),
        },
    }

    Ok(())
}