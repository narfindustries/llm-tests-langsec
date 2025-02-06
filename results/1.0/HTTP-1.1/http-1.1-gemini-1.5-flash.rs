use nom::{
    bytes::complete::{take_while, take_until},
    character::complete::{alpha1, alphanumeric1, line_ending, space1},
    combinator::{map_res, recognize},
    multi::separated_list0,
    sequence::{pair, terminated},
    IResult,
};
use std::fs;
use std::path::Path;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
struct HttpRequest {
    method: String,
    path: String,
    http_version: String,
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct HttpResponse {
    http_version: String,
    status_code: u16,
    reason_phrase: String,
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}

fn is_token_char(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'-' || c == b'_' || c == b'.' || c == b'!' || c == b'#' || c == b'$' || c == b'%' || c == b'&' || c == b'\'' || c == b'*' || c == b'+' || c == b'|' || c == b'^' || c == b'='
}

fn http_token(input: &[u8]) -> IResult<&[u8], String> {
    map_res(
        recognize(pair(
            alpha1,
            take_while(is_token_char),
        )),
        |bytes: &[u8]| String::from_utf8(bytes.to_vec()),
    )(input)
}

fn http_header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, name) = terminated(http_token, space1)(input)?;
    let (input, value) = take_until("\r\n")(input)?;
    Ok((
        input,
        (String::from_utf8_lossy(name).into_owned(), String::from_utf8_lossy(value).into_owned()),
    ))
}

fn http_headers(input: &[u8]) -> IResult<&[u8], Vec<(String, String)>> {
    separated_list0(line_ending, http_header)(input)
}

fn http_request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    let (input, method) = terminated(http_token, space1)(input)?;
    let (input, path) = terminated(take_until("HTTP"), space1)(input)?;
    let (input, version) = take_until("\r\n")(input)?;

    Ok((
        input,
        (
            method,
            String::from_utf8_lossy(path).into_owned(),
            String::from_utf8_lossy(version).into_owned(),
        ),
    ))
}

fn http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, path, version)) = http_request_line(input)?;
    let (input, headers) = http_headers(input)?;
    let (input, body) = take_until("\r\n")(input)?;

    Ok((
        input,
        HttpRequest {
            method,
            path,
            http_version: version,
            headers,
            body: body.to_vec(),
        },
    ))
}

fn http_status_line(input: &[u8]) -> IResult<&[u8], (String, u16, String)> {
    let (input, version) = terminated(take_until(" "), space1)(input)?;
    let (input, code_str) = recognize(alphanumeric1)(input)?;
    let code = u16::from_str(std::str::from_utf8(code_str).unwrap()).unwrap();
    let (input, reason) = take_until("\r\n")(input)?;

    Ok((
        input,
        (
            String::from_utf8_lossy(version).into_owned(),
            code,
            String::from_utf8_lossy(reason).into_owned(),
        ),
    ))
}

fn http_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    let (input, (version, code, reason)) = http_status_line(input)?;
    let (input, headers) = http_headers(input)?;
    let (input, body) = take_until("\r\n")(input)?;

    Ok((
        input,
        HttpResponse {
            http_version: version,
            status_code: code,
            reason_phrase: reason,
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
        Ok((_, req)) => println!("Request: {:?}", req),
        Err(e) => println!("Error parsing request: {:?}", e),
    }

    match http_response(&contents) {
        Ok((_, res)) => println!("Response: {:?}", res),
        Err(e) => println!("Error parsing response: {:?}", e),
    }
}