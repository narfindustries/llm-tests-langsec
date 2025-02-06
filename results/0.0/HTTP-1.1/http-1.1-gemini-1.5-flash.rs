use nom::{
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit1, line_ending, space0, space1},
    combinator::{map_res, recognize},
    multi::many0,
    sequence::{pair, preceded, separated_pair, terminated},
    IResult,
};
use std::fs;
use std::path::Path;
use std::str;

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
    (c >= b'!' && c <= b' ') || (c >= b'#' && c <= b'[' ) || (c >= b']' && c <= b'~')
}

fn token(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while1(is_token_char), str::from_utf8)(input)
        .map(|(i, o)| (i, o.to_string()))
}

fn header_name(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while1(|c: u8| c.is_ascii_alphanumeric() || c == b'-'), str::from_utf8)(input)
        .map(|(i, o)| (i, o.to_string()))
}

fn header_value(input: &[u8]) -> IResult<&[u8], String> {
    map_res(take_while1(|c: u8| c != b'\r' && c != b'\n'), str::from_utf8)(input)
        .map(|(i, o)| (i, o.to_string()))
}

fn header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    separated_pair(header_name, space1, header_value)(input)
}

fn headers(input: &[u8]) -> IResult<&[u8], Vec<(String, String)>> {
    many0(terminated(header, line_ending))(input)
}

fn http_version(input: &[u8]) -> IResult<&[u8], String> {
    recognize(pair(tag(b"HTTP/"), pair(digit1, char(b'.'))))(input)
        .map(|(i, o)| (i, str::from_utf8(o).unwrap().to_string()))
}

fn request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    separated_pair(
        token,
        space1,
        separated_pair(take_while1(|c: u8| c != b' '), space1, http_version),
    )(input)
}

fn request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, path, http_version)) = preceded(space0, request_line)(input)?;
    let (input, headers) = preceded(line_ending, headers)(input)?;
    let (input, body) = take_while(|c: u8| c != 0)(input)?;
    Ok((
        input,
        HttpRequest {
            method: method.to_string(),
            path: path.to_string(),
            http_version: http_version.to_string(),
            headers,
            body: body.to_vec(),
        },
    ))
}

fn status_line(input: &[u8]) -> IResult<&[u8], (String, u16, String)> {
    separated_pair(
        http_version,
        space1,
        pair(
            map_res(digit1, |s: &[u8]| {
                let s = str::from_utf8(s).unwrap();
                s.parse::<u16>().map_err(|_| nom::Err::Failure(("".to_string(), nom::error::ErrorKind::Parse)))
            }),
            take_while1(|c: u8| c != b'\r' && c != b'\n'),
        ),
    )(input)
}

fn response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    let (input, (http_version, status_code, reason_phrase)) = preceded(space0, status_line)(input)?;
    let (input, headers) = preceded(line_ending, headers)(input)?;
    let (input, body) = take_while(|c: u8| c != 0)(input)?;
    Ok((
        input,
        HttpResponse {
            http_version: http_version.to_string(),
            status_code,
            reason_phrase: reason_phrase.to_string(),
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

    match request(&contents) {
        Ok((_, req)) => println!("Request: {:?}", req),
        Err(e) => println!("Error parsing request: {:?}", e),
    }

    match response(&contents) {
        Ok((_, res)) => println!("Response: {:?}", res),
        Err(e) => println!("Error parsing response: {:?}", e),
    }
}