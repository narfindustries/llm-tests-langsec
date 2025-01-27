use std::fs::File;
use std::io::{self, Read};
use std::env;
use nom::{
    IResult,
    bytes::complete::{take_until, take_while, tag, take},
    character::complete::{char, digit1, space0, space1, alphanumeric1},
    combinator::{map_res, opt},
    sequence::{tuple, preceded, terminated},
    multi::many0,
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
    c.is_alphanumeric() || c == '-'
}

fn parse_method(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take_while(is_token), std::str::from_utf8)(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take_while(|c| c != b' '), std::str::from_utf8)(input)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take_while(|c| c != b'\r'), std::str::from_utf8)(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, key) = map_res(take_while(|c| c != b':'), std::str::from_utf8)(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = map_res(take_until("\r\n"), std::str::from_utf8)(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((input, (key.to_string(), value.to_string())))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<(String, String)>> {
    many0(parse_header)(input)
}

fn parse_body(input: &[u8]) -> IResult<&[u8], Option<String>> {
    if input.is_empty() {
        Ok((input, None))
    } else {
        let (input, body) = map_res(take(input.len()), std::str::from_utf8)(input)?;
        Ok((input, Some(body.to_string())))
    }
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, _, uri, _, version, _)) = tuple((
        parse_method,
        space1,
        parse_uri,
        space1,
        parse_version,
        tag("\r\n"),
    ))(input)?;

    let (input, headers) = parse_headers(input)?;
    let (input, _) = tag("\r\n")(input)?;
    let (input, body) = parse_body(input)?;

    Ok((
        input,
        HttpRequest {
            method: method.to_string(),
            uri: uri.to_string(),
            version: version.to_string(),
            headers,
            body,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_request(&buffer) {
        Ok((_, request)) => println!("{:?}", request),
        Err(e) => eprintln!("Failed to parse HTTP request: {:?}", e),
    }

    Ok(())
}