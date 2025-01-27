use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::{take_while, tag, take_until},
    character::complete::{digit1, space1, alphanumeric1},
    sequence::{tuple, preceded, terminated},
    multi::separated_list0,
    combinator::{map_res, opt},
    branch::alt,
};

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<(String, String)>,
    body: Option<String>,
}

fn is_token(c: char) -> bool {
    c.is_ascii_alphanumeric() || "!#$%&'*+-.^_`|~".contains(c)
}

fn is_header_value(c: char) -> bool {
    c.is_ascii_graphic() || c == ' '
}

fn parse_method(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take_while(is_token), std::str::from_utf8)(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take_while(|c| c != b' '), std::str::from_utf8)(input)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(preceded(tag("HTTP/"), tuple((digit1, tag("."), digit1))), |(major, _, minor)| {
        Ok(format!("{}.{}", std::str::from_utf8(major)?, std::str::from_utf8(minor)?))
    })(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, (name, _, value)) = tuple((
        map_res(take_while(is_token), std::str::from_utf8),
        tag(": "),
        map_res(take_while(is_header_value), std::str::from_utf8)
    ))(input)?;
    Ok((input, (name.to_string(), value.trim_end().to_string())))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<(String, String)>> {
    separated_list0(tag("\r\n"), parse_header)(input)
}

fn parse_body(input: &[u8]) -> IResult<&[u8], Option<String>> {
    opt(map_res(take_while(|_| true), std::str::from_utf8))(input).map(|(next_input, opt_str)| (next_input, opt_str.map(|s| s.to_string())))
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, _, uri, _, version, _, headers, _, body)) = tuple((
        parse_method,
        space1,
        parse_uri,
        space1,
        parse_version,
        tag("\r\n"),
        parse_headers,
        tag("\r\n\r\n"),
        parse_body
    ))(input)?;

    Ok((input, HttpRequest {
        method: method.to_string(),
        uri: uri.to_string(),
        version: version.to_string(),
        headers,
        body,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Could not open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");

    match parse_request(&buffer) {
        Ok((_, request)) => println!("{:?}", request),
        Err(e) => eprintln!("Failed to parse HTTP request: {:?}", e),
    }
}