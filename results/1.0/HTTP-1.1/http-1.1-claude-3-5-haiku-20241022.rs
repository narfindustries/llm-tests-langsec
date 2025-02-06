use nom::{
    IResult,
    bytes::complete::{tag, take_while, take_while1},
    sequence::{tuple, terminated},
    character::complete::{space1, crlf},
    multi::many0,
    combinator::opt,
};
use std::str::from_utf8;
use std::env;
use std::fs;

#[derive(Debug)]
struct HttpRequest<'a> {
    method: &'a str,
    uri: &'a str,
    version: &'a str,
    headers: Vec<Header<'a>>,
    body: Option<&'a [u8]>,
}

#[derive(Debug)]
struct Header<'a> {
    name: &'a str,
    value: &'a str,
}

fn is_token_char(c: u8) -> bool {
    matches!(c as char, 'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' | '.' | '~')
}

fn parse_method(input: &[u8]) -> IResult<&[u8], &str> {
    let methods = &["GET", "POST", "PUT", "DELETE", "HEAD", "OPTIONS", "TRACE", "CONNECT"];
    let (remaining, method) = take_while1(|c: u8| (c as char).is_ascii_uppercase())(input)?;
    let method_str = from_utf8(method).unwrap();
    
    if methods.contains(&method_str) {
        Ok((remaining, method_str))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
    }
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], &str> {
    let (remaining, uri) = take_while1(|c: u8| c != b' ')(input)?;
    Ok((remaining, from_utf8(uri).unwrap()))
}

fn parse_http_version(input: &[u8]) -> IResult<&[u8], &str> {
    let (remaining, version) = terminated(
        tag("HTTP/1.1"),
        crlf
    )(input)?;
    Ok((remaining, "HTTP/1.1"))
}

fn parse_header_name(input: &[u8]) -> IResult<&[u8], &str> {
    let (remaining, name) = take_while1(is_token_char)(input)?;
    Ok((remaining, from_utf8(name).unwrap()))
}

fn parse_header_value(input: &[u8]) -> IResult<&[u8], &str> {
    let (remaining, value) = take_while(|c: u8| c != b'\r')(input)?;
    Ok((remaining, from_utf8(value).unwrap().trim()))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    let (remaining, (name, _, value, _)) = tuple((
        parse_header_name,
        tag(": "),
        parse_header_value,
        crlf
    ))(input)?;

    Ok((remaining, Header { name, value }))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<Header>> {
    many0(parse_header)(input)
}

fn parse_body(input: &[u8]) -> IResult<&[u8], Option<&[u8]>> {
    opt(take_while(|_| true))(input)
}

fn parse_http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (remaining, (method, _, uri, _, _version)) = tuple((
        parse_method,
        space1,
        parse_uri,
        space1,
        parse_http_version
    ))(input)?;

    let (remaining, headers) = parse_headers(remaining)?;
    let (remaining, body) = parse_body(remaining)?;

    Ok((
        remaining, 
        HttpRequest {
            method,
            uri,
            version: "HTTP/1.1",
            headers,
            body
        }
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input_file = &args[1];
    let input_data = fs::read(input_file)?;

    match parse_http_request(&input_data) {
        Ok((_, request)) => {
            println!("{:#?}", request);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1)
        }
    }
}