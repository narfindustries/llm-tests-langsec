use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, crlf, digit1, line_ending, space0, space1},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

type Header = (String, String);

fn parse_method(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while1(|c| c.is_ascii_alphabetic())(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while1(|c| c != b' ' && c != b'\r' && c != b'\n')(input)
}

fn parse_http_version(input: &[u8]) -> IResult<&[u8], &[u8]> {
    recognize(tuple((tag("HTTP/"), digit1, char('.'), digit1)))(input)
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8], &[u8])> {
    tuple((
        parse_method,
        preceded(space1, parse_uri),
        preceded(space1, parse_http_version),
    ))(input)
}

fn parse_status_code(input: &[u8]) -> IResult<&[u8], &[u8]> {
    digit1(input)
}

fn parse_reason_phrase(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while(|c| c != b'\r' && c != b'\n')(input)
}

fn parse_status_line(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8], &[u8])> {
    tuple((
        parse_http_version,
        preceded(space1, parse_status_code),
        preceded(space1, parse_reason_phrase),
    ))(input)
}

fn parse_header_name(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while1(|c| c.is_ascii_alphabetic() || c == b'-')(input)
}

fn parse_header_value(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while(|c| c != b'\r' && c != b'\n')(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    map(
        separated_pair(
            parse_header_name,
            tuple((space0, char(':'), space0)),
            parse_header_value,
        ),
        |(name, value)| (String::from_utf8_lossy(name).to_string(), String::from_utf8_lossy(value).to_string()),
    )(input)
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<Header>> {
    many0(terminated(parse_header, crlf))(input)
}

fn parse_message_body(input: &[u8]) -> IResult<&[u8], &[u8]> {
    Ok((&[], input))
}

fn parse_request(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8], &[u8], Vec<Header>, &[u8])> {
    tuple((
        parse_request_line,
        crlf,
        parse_headers,
        crlf,
        parse_message_body,
    ))(input)
}

fn parse_response(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8], &[u8], Vec<Header>, &[u8])> {
    tuple((
        parse_status_line,
        crlf,
        parse_headers,
        crlf,
        parse_message_body,
    ))(input)
}

fn parse_http_message(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8], &[u8], Vec<Header>, &[u8])> {
    alt((parse_request, parse_response))(input)
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

    match parse_http_message(&buffer) {
        Ok((_, (method_or_version, uri_or_status, http_version_or_reason, headers, body))) => {
            println!("Method/Version: {:?}", String::from_utf8_lossy(method_or_version));
            println!("URI/Status: {:?}", String::from_utf8_lossy(uri_or_status));
            println!("HTTP Version/Reason: {:?}", String::from_utf8_lossy(http_version_or_reason));
            println!("Headers:");
            for (name, value) in headers {
                println!("  {}: {}", name, value);
            }
            println!("Body: {:?}", String::from_utf8_lossy(body));
        }
        Err(e) => eprintln!("Failed to parse HTTP message: {:?}", e),
    }

    Ok(())
}