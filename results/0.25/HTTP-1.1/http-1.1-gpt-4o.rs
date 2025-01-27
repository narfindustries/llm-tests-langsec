use nom::{
    bytes::complete::{tag, take_until, take_while},
    character::complete::{digit1, space1},
    combinator::{map_res, opt},
    multi::many0,
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::str;
use std::str::FromStr;

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<(String, String)>,
    body: Option<String>,
}

fn is_token(c: char) -> bool {
    c.is_alphanumeric() || "-!#$%&'*+.^_`|~".contains(c)
}

fn parse_method(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take_while(is_token), str::from_utf8)(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(take_while(|c| c != b' '), str::from_utf8)(input)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(
        preceded(tag("HTTP/"), take_while(|c| c.is_ascii_digit() || c == b'.')),
        str::from_utf8,
    )(input)
}

fn parse_header_line(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, (name, _, value)) = tuple((
        map_res(take_while(is_token), str::from_utf8),
        tag(": "),
        map_res(take_until("\r\n"), str::from_utf8),
    ))(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((input, (name.to_string(), value.to_string())))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<(String, String)>> {
    many0(parse_header_line)(input)
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

    let (input, body) = opt(map_res(take_while(|_| true), str::from_utf8))(input)?;

    Ok((
        input,
        HttpRequest {
            method: method.to_string(),
            uri: uri.to_string(),
            version: version.to_string(),
            headers,
            body: body.map(|s| s.to_string()),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_request(&buffer) {
        Ok((_, request)) => println!("{:#?}", request),
        Err(e) => eprintln!("Failed to parse HTTP request: {:?}", e),
    }

    Ok(())
}