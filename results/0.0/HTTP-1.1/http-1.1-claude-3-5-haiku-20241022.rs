use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1, take_while_m_n},
    character::complete::{char, digit1, multispace0, multispace1, not_line_ending},
    combinator::{map, map_res},
    multi::many0,
    sequence::{delimited, separated_pair, terminated},
    IResult,
};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::str;

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

fn parse_method(input: &[u8]) -> IResult<&[u8], String> {
    alt((
        map(tag(b"GET"), |_| "GET".to_string()),
        map(tag(b"POST"), |_| "POST".to_string()),
        map(tag(b"HEAD"), |_| "HEAD".to_string()),
        map(tag(b"PUT"), |_| "PUT".to_string()),
        map(tag(b"DELETE"), |_| "DELETE".to_string()),
        map(tag(b"CONNECT"), |_| "CONNECT".to_string()),
        map(tag(b"OPTIONS"), |_| "OPTIONS".to_string()),
        map(tag(b"TRACE"), |_| "TRACE".to_string()),
    ))(input)
}

fn parse_http_version(input: &[u8]) -> IResult<&[u8], String> {
    map(
        separated_pair(
            tag(b"HTTP"),
            char('/'),
            separated_pair(digit1, char('.'), digit1),
        ),
        |(_, (major, minor))| {
            format!(
                "HTTP/{}.{}",
                str::from_utf8(major).unwrap(),
                str::from_utf8(minor).unwrap()
            )
        },
    )(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    map(
        separated_pair(
            take_while1(|c: u8| c != b':'),
            delimited(multispace0, char(':'), multispace0),
            not_line_ending,
        ),
        |(key, value)| {
            (
                str::from_utf8(key).unwrap().to_string(),
                str::from_utf8(value).unwrap().to_string(),
            )
        },
    )(input)
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], HashMap<String, String>> {
    map(many0(terminated(parse_header, tag(b"\r\n"))), |headers| {
        headers.into_iter().collect()
    })(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], String> {
    map(take_while1(|c: u8| c != b' '), |uri| {
        str::from_utf8(uri).unwrap().to_string()
    })(input)
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, method) = parse_method(input)?;
    let (input, _) = multispace1(input)?;
    let (input, uri) = parse_uri(input)?;
    let (input, _) = multispace1(input)?;
    let (input, version) = parse_http_version(input)?;
    let (input, _) = tag(b"\r\n")(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, _) = tag(b"\r\n")(input)?;
    
    let content_length = headers.get("Content-Length")
        .and_then(|s| s.parse::<usize>().ok());
    
    let body = content_length.map(|len| input[..len].to_vec());

    Ok((
        &input[content_length.unwrap_or(0)..],
        HttpRequest {
            method: method.to_string(),
            uri,
            version,
            headers,
            body,
        },
    ))
}

fn parse_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    let (input, version) = parse_http_version(input)?;
    let (input, _) = multispace1(input)?;
    let (input, status_code) = map_res(digit1, |s: &[u8]| str::from_utf8(s).unwrap().parse::<u16>())(input)?;
    let (input, _) = multispace1(input)?;
    let (input, status_text) = not_line_ending(input)?;
    let (input, _) = tag(b"\r\n")(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, _) = tag(b"\r\n")(input)?;
    
    let content_length = headers.get("Content-Length")
        .and_then(|s| s.parse::<usize>().ok());
    
    let body = content_length.map(|len| input[..len].to_vec());

    Ok((
        &input[content_length.unwrap_or(0)..],
        HttpResponse {
            version,
            status_code,
            status_text: str::from_utf8(status_text).unwrap().to_string(),
            headers,
            body,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input = fs::read(&args[1]).expect("Unable to read file");

    match parse_request(&input) {
        Ok((_, request)) => {
            println!("Parsed Request: {:?}", request);
        }
        Err(_) => {
            match parse_response(&input) {
                Ok((_, response)) => {
                    println!("Parsed Response: {:?}", response);
                }
                Err(e) => {
                    eprintln!("Parsing failed: {:?}", e);
                    std::process::exit(1);
                }
            }
        }
    }
}