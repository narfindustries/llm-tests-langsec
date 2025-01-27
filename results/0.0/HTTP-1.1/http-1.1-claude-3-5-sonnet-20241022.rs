use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{char, digit1, space0, space1},
    combinator::{map, map_res, opt},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::{collections::HashMap, env, fs, str};

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

fn is_token_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || "!#$%&'*+-.^_`|~".contains(c)
}

fn is_header_value_char(c: char) -> bool {
    c.is_ascii() && c != '\r' && c != '\n'
}

fn method(input: &[u8]) -> IResult<&[u8], &str> {
    let (input, method) = take_while1(is_token_char)(input)?;
    Ok((input, str::from_utf8(method).unwrap()))
}

fn uri(input: &[u8]) -> IResult<&[u8], &str> {
    let (input, uri) = take_while1(|c| c != b' ')(input)?;
    Ok((input, str::from_utf8(uri).unwrap()))
}

fn http_version(input: &[u8]) -> IResult<&[u8], &str> {
    let (input, _) = tag("HTTP/")(input)?;
    let (input, version) = take_while1(|c| c.is_ascii_digit() || c == b'.')(input)?;
    Ok((input, str::from_utf8(version).unwrap()))
}

fn header_name(input: &[u8]) -> IResult<&[u8], &str> {
    let (input, name) = take_while1(is_token_char)(input)?;
    Ok((input, str::from_utf8(name).unwrap()))
}

fn header_value(input: &[u8]) -> IResult<&[u8], &str> {
    let (input, value) = take_while1(is_header_value_char)(input)?;
    Ok((input, str::from_utf8(value.trim()).unwrap()))
}

fn header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, (name, _, value, _)) = tuple((
        header_name,
        tag(":"),
        header_value,
        tag("\r\n"),
    ))(input)?;
    Ok((input, (name.to_string(), value.to_string())))
}

fn headers(input: &[u8]) -> IResult<&[u8], HashMap<String, String>> {
    let (input, headers) = many0(header)(input)?;
    Ok((input, headers.into_iter().collect()))
}

fn request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    let (input, (method, _, uri, _, version, _)) = tuple((
        method,
        space1,
        uri,
        space1,
        http_version,
        tag("\r\n"),
    ))(input)?;
    Ok((input, (method.to_string(), uri.to_string(), version.to_string())))
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, uri, version)) = request_line(input)?;
    let (input, headers) = headers(input)?;
    let (input, _) = tag("\r\n")(input)?;
    let (input, body) = if let Some(length) = headers.get("Content-Length") {
        let length: usize = length.parse().unwrap();
        let (input, body) = take_while(|_| true)(input)?;
        (input, Some(body.to_vec()))
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

fn status_code(input: &[u8]) -> IResult<&[u8], u16> {
    map_res(digit1, |s: &[u8]| {
        str::from_utf8(s).unwrap().parse::<u16>()
    })(input)
}

fn status_text(input: &[u8]) -> IResult<&[u8], &str> {
    let (input, text) = take_until("\r\n")(input)?;
    Ok((input, str::from_utf8(text).unwrap()))
}

fn status_line(input: &[u8]) -> IResult<&[u8], (String, u16, String)> {
    let (input, (version, _, code, _, text, _)) = tuple((
        http_version,
        space1,
        status_code,
        space1,
        status_text,
        tag("\r\n"),
    ))(input)?;
    Ok((
        input,
        (version.to_string(), code, text.trim().to_string()),
    ))
}

fn parse_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    let (input, (version, status_code, status_text)) = status_line(input)?;
    let (input, headers) = headers(input)?;
    let (input, _) = tag("\r\n")(input)?;
    let (input, body) = if let Some(length) = headers.get("Content-Length") {
        let length: usize = length.parse().unwrap();
        let (input, body) = take_while(|_| true)(input)?;
        (input, Some(body.to_vec()))
    } else {
        (input, None)
    };

    Ok((
        input,
        HttpResponse {
            version,
            status_code,
            status_text,
            headers,
            body,
        },
    ))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input = fs::read(&args[1])?;
    
    match parse_request(&input) {
        Ok((_, request)) => println!("Request: {:#?}", request),
        Err(e) => match parse_response(&input) {
            Ok((_, response)) => println!("Response: {:#?}", response),
            Err(_) => eprintln!("Failed to parse HTTP message: {:?}", e),
        },
    }

    Ok(())
}