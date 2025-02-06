use nom::{
    bytes::complete::{tag, take_while1},
    character::complete::{alpha1, alphanumeric1, char, digit1, line_ending, space0, space1},
    combinator::{recognize},
    multi::many0,
    sequence::{delimited, pair, terminated},
    IResult,
};
use std::fs;
use std::path::Path;

#[derive(Debug, PartialEq)]
enum HttpMethod {
    Get,
    Post,
    Put,
    Delete,
    Head,
    Options,
    Connect,
    Trace,
    Patch,
    Other(String),
}

#[derive(Debug, PartialEq)]
struct HttpRequest {
    method: HttpMethod,
    path: String,
    http_version: String,
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct HttpResponse {
    http_version: String,
    status_code: u16,
    status_text: String,
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}

fn http_method(input: &[u8]) -> IResult<&[u8], HttpMethod> {
    let (input, method) = recognize(pair(alpha1, many0(alphanumeric1)))(input)?;
    let method_str = std::str::from_utf8(method).unwrap();
    Ok((
        input,
        match method_str.to_lowercase().as_str() {
            "get" => HttpMethod::Get,
            "post" => HttpMethod::Post,
            "put" => HttpMethod::Put,
            "delete" => HttpMethod::Delete,
            "head" => HttpMethod::Head,
            "options" => HttpMethod::Options,
            "connect" => HttpMethod::Connect,
            "trace" => HttpMethod::Trace,
            "patch" => HttpMethod::Patch,
            _ => HttpMethod::Other(method_str.to_string()),
        },
    ))
}

fn http_header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, name) = terminated(
        take_while1(|c: u8| c != b':' && !c.is_ascii_whitespace()),
        char(':'),
    )(input)?;
    let (input, value) = delimited(space0, take_while1(|c: u8| c != b'\r'), space0)(input)?;
    Ok((
        input,
        (
            std::str::from_utf8(name).unwrap().to_string(),
            std::str::from_utf8(value).unwrap().trim().to_string(),
        ),
    ))
}

fn http_headers(input: &[u8]) -> IResult<&[u8], Vec<(String, String)>> {
    many0(terminated(http_header, line_ending))(input)
}

fn http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, method) = terminated(http_method, space1)(input)?;
    let (input, path) = terminated(take_while1(|c: u8| c != b' '), space1)(input)?;
    let (input, http_version) = terminated(
        recognize(pair(tag("HTTP/"), digit1)),
        line_ending,
    )(input)?;
    let (input, headers) = http_headers(input)?;
    let (input, body) = take_while1(|_| true)(input)?;

    Ok((
        input,
        HttpRequest {
            method,
            path: std::str::from_utf8(path).unwrap().to_string(),
            http_version: std::str::from_utf8(http_version).unwrap().to_string(),
            headers,
            body: body.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let contents = fs::read(path).expect("Failed to read file");

    match http_request(&contents) {
        Ok((_, request)) => println!("{:#?}", request),
        Err(e) => eprintln!("Error parsing HTTP request: {:?}", e),
    }
}
