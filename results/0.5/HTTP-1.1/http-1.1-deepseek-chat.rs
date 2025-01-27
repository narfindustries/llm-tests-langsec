use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{char, crlf, digit1, line_ending, space0, space1},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<Header>,
    body: Option<Vec<u8>>,
}

#[derive(Debug)]
struct Header {
    name: String,
    value: String,
}

fn parse_method(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c| matches!(c, b'A'..=b'Z' | b'a'..=b'z' | b'-')),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c| c != b' ' && c != b'\r' && c != b'\n'),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], String> {
    map(
        preceded(
            tag("HTTP/"),
            recognize(tuple((digit1, char('.'), digit1))),
        ),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    map(
        separated_pair(
            take_while1(|c| c != b':'),
            preceded(space0, char(':')),
            preceded(space0, take_until("\r\n")),
        ),
        |(name, value): (&[u8], &[u8])| Header {
            name: String::from_utf8_lossy(name).to_string(),
            value: String::from_utf8_lossy(value).to_string(),
        },
    )(input)
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<Header>> {
    many0(terminated(parse_header, crlf))(input)
}

fn parse_body(input: &[u8], content_length: usize) -> IResult<&[u8], Vec<u8>> {
    map(take(content_length), |s: &[u8]| s.to_vec())(input)
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, uri, version)) = tuple((
        terminated(parse_method, space1),
        terminated(parse_uri, space1),
        terminated(parse_version, crlf),
    ))(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, _) = crlf(input)?;

    let content_length = headers
        .iter()
        .find(|h| h.name.to_lowercase() == "content-length")
        .and_then(|h| h.value.parse::<usize>().ok())
        .unwrap_or(0);

    let (input, body) = if content_length > 0 {
        let (input, body) = parse_body(input, content_length)?;
        (input, Some(body))
    } else {
        (input, None)
    };

    Ok((
        input,
        HttpRequest {
            method,
            uri,
            version,
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
        Ok((_, request)) => println!("{:#?}", request),
        Err(e) => eprintln!("Failed to parse request: {:?}", e),
    }

    Ok(())
}