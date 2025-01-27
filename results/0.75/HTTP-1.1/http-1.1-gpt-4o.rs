use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::{tag, take_until, take_while},
    character::complete::{digit1, space1, not_line_ending},
    sequence::{tuple, preceded, terminated},
    multi::many0,
    combinator::{opt, map_res},
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

fn parse_method(input: &[u8]) -> IResult<&[u8], &str> {
    alt((
        tag("GET"),
        tag("POST"),
        tag("PUT"),
        tag("DELETE"),
        tag("OPTIONS"),
        tag("HEAD"),
        tag("PATCH"),
        tag("CONNECT"),
        tag("TRACE"),
    ))(input)
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (&str, &str, &str)> {
    tuple((
        parse_method,
        preceded(space1, take_until(" ")),
        preceded(space1, not_line_ending),
    ))(input)
}

fn parse_header_line(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, (name, _, value)) = tuple((
        map_res(take_until(":"), std::str::from_utf8),
        tag(": "),
        map_res(not_line_ending, std::str::from_utf8),
    ))(input)?;
    Ok((input, (name.to_string(), value.to_string())))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<(String, String)>> {
    many0(terminated(parse_header_line, opt(tag("\r\n"))))(input)
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, uri, version)) = parse_request_line(input)?;
    let (input, _) = tag("\r\n")(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, _) = tag("\r\n")(input)?;
    let body = if let Some(_) = headers.iter().find(|(name, _)| name.to_lowercase() == "content-length") {
        let (input, body) = opt(map_res(not_line_ending, std::str::from_utf8))(input)?;
        (input, body.map(|s| s.to_string()))
    } else {
        (input, None)
    };
    Ok((input, HttpRequest {
        method: method.to_string(),
        uri: std::str::from_utf8(uri).unwrap().to_string(),
        version: version.to_string(),
        headers,
        body: body.1,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read data");

    match parse_request(&buffer) {
        Ok((_, request)) => {
            println!("{:#?}", request);
        },
        Err(e) => {
            eprintln!("Failed to parse HTTP request: {:?}", e);
        }
    }
}