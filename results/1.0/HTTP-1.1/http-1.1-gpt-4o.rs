extern crate nom;

use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{alphanumeric1, crlf, space1},
    combinator::map_res,
    multi::separated_list0,
    sequence::{preceded, terminated, tuple},
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<HttpHeader>,
}

#[derive(Debug)]
struct HttpHeader {
    name: String,
    value: String,
}

fn parse_http_method(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(alphanumeric1, std::str::from_utf8)(input)
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (&str, &str, &str)> {
    tuple((
        parse_http_method,
        preceded(space1, map_res(take_until(" "), std::str::from_utf8)),
        preceded(space1, map_res(take_until("\r\n"), std::str::from_utf8)),
    ))(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], HttpHeader> {
    let (input, (name, _, value)) = tuple((alphanumeric1, tag(": "), take_until("\r\n")))(input)?;
    Ok((
        input,
        HttpHeader {
            name: String::from_utf8(name.to_vec()).unwrap(),
            value: String::from_utf8(value.to_vec()).unwrap(),
        },
    ))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<HttpHeader>> {
    separated_list0(crlf, parse_header)(input)
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, uri, version)) = terminated(parse_request_line, crlf)(input)?;
    let (input, headers) = terminated(parse_headers, crlf)(input)?;

    Ok((
        input,
        HttpRequest {
            method: method.to_string(),
            uri: uri.to_string(),
            version: version.to_string(),
            headers,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        return;
    }

    let file_path = &args[1];
    let mut file = File::open(file_path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)
        .expect("Failed to read file");

    match parse_request(&buffer) {
        Ok((_, request)) => println!("{:#?}", request),
        Err(e) => eprintln!("Error parsing HTTP request: {:?}", e),
    }
}