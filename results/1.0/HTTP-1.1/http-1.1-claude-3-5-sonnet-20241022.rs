use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{char, digit1, space0, space1},
    combinator::{map, map_res, opt},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::{env, fs::File, io::Read, str::FromStr};

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<(String, String)>,
    body: Option<Vec<u8>>,
}

#[derive(Debug)]
struct HttpResponse {
    version: String,
    status_code: u16,
    status_text: String,
    headers: Vec<(String, String)>,
    body: Option<Vec<u8>>,
}

fn is_token_char(c: char) -> bool {
    !matches!(c, '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"'
        | '/' | '[' | ']' | '?' | '=' | '{' | '}' | ' ' | '\t')
}

fn method(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c| is_token_char(c as char)),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn uri(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c| c != b' '),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn http_version(input: &[u8]) -> IResult<&[u8], String> {
    map(
        preceded(
            tag("HTTP/"),
            take_while1(|c| c != b'\r' && c != b'\n'),
        ),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn header_name(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c| is_token_char(c as char)),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn header_value(input: &[u8]) -> IResult<&[u8], String> {
    map(
        delimited(
            char(':'),
            preceded(
                space0,
                take_while(|c| c != b'\r' && c != b'\n'),
            ),
            tag("\r\n"),
        ),
        |s: &[u8]| String::from_utf8_lossy(s).trim().to_string(),
    )(input)
}

fn header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    pair(header_name, header_value)(input)
}

fn headers(input: &[u8]) -> IResult<&[u8], Vec<(String, String)>> {
    terminated(many0(header), tag("\r\n"))(input)
}

fn request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    terminated(
        tuple((
            terminated(method, space1),
            terminated(uri, space1),
            http_version,
        )),
        tag("\r\n"),
    )(input)
}

fn status_line(input: &[u8]) -> IResult<&[u8], (String, u16, String)> {
    terminated(
        tuple((
            terminated(http_version, space1),
            terminated(map_res(digit1, |s: &[u8]| {
                String::from_utf8_lossy(s).parse::<u16>()
            }), space1),
            map(
                take_while(|c| c != b'\r' && c != b'\n'),
                |s: &[u8]| String::from_utf8_lossy(s).to_string(),
            ),
        )),
        tag("\r\n"),
    )(input)
}

fn http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, uri, version)) = request_line(input)?;
    let (input, headers) = headers(input)?;
    let (input, body) = opt(take_until(""))(input)?;

    Ok((
        input,
        HttpRequest {
            method,
            uri,
            version,
            headers,
            body: body.map(|b| b.to_vec()),
        },
    ))
}

fn http_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    let (input, (version, status_code, status_text)) = status_line(input)?;
    let (input, headers) = headers(input)?;
    let (input, body) = opt(take_until(""))(input)?;

    Ok((
        input,
        HttpResponse {
            version,
            status_code,
            status_text,
            headers,
            body: body.map(|b| b.to_vec()),
        },
    ))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;

    match http_request(&contents) {
        Ok((_, request)) => println!("Request: {:?}", request),
        Err(e) => match http_response(&contents) {
            Ok((_, response)) => println!("Response: {:?}", response),
            Err(_) => eprintln!("Failed to parse HTTP message: {:?}", e),
        },
    }

    Ok(())
}