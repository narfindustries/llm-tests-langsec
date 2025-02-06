use nom::{
    IResult,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{digit1, space1},
    combinator::{map, map_res},
    sequence::tuple,
    branch::alt,
    multi::many0,
};
use std::collections::HashMap;
use std::env;
use std::fs;

#[derive(Debug)]
struct HttpMessage {
    start_line: StartLine,
    headers: HashMap<String, String>,
    body: Option<Vec<u8>>,
}

#[derive(Debug)]
enum StartLine {
    Request(RequestLine),
    Response(StatusLine),
}

#[derive(Debug)]
struct RequestLine {
    method: String,
    request_uri: String,
    version: String,
}

#[derive(Debug)]
struct StatusLine {
    version: String,
    status_code: u16,
    reason_phrase: String,
}

fn is_token_char(c: u8) -> bool {
    match c {
        33..=126 => !b"()<>@,;:\\\"/[]?={} \t".contains(&c),
        _ => false,
    }
}

fn token(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while1(is_token_char)(input)
}

fn header_name(input: &[u8]) -> IResult<&[u8], &[u8]> {
    token(input)
}

fn header_value(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while(|c| c != b'\r' && c != b'\n')(input)
}

fn header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, (name, _, value, _)) = tuple((
        header_name,
        tag(": "),
        header_value,
        tag("\r\n"),
    ))(input)?;
    
    Ok((input, (
        String::from_utf8_lossy(name).into_owned(),
        String::from_utf8_lossy(value).into_owned().trim().to_string()
    )))
}

fn request_line(input: &[u8]) -> IResult<&[u8], RequestLine> {
    let (input, (method, _, uri, _, version, _)) = tuple((
        token,
        space1,
        take_until(" "),
        space1,
        take_until("\r"),
        tag("\r\n"),
    ))(input)?;

    Ok((input, RequestLine {
        method: String::from_utf8_lossy(method).into_owned(),
        request_uri: String::from_utf8_lossy(uri).into_owned(),
        version: String::from_utf8_lossy(version).into_owned(),
    }))
}

fn status_line(input: &[u8]) -> IResult<&[u8], StatusLine> {
    let (input, (version, _, code, _, reason, _)) = tuple((
        take_until(" "),
        space1,
        map_res(digit1, |s: &[u8]| {
            String::from_utf8_lossy(s).parse::<u16>()
        }),
        space1,
        take_until("\r"),
        tag("\r\n"),
    ))(input)?;

    Ok((input, StatusLine {
        version: String::from_utf8_lossy(version).into_owned(),
        status_code: code,
        reason_phrase: String::from_utf8_lossy(reason).into_owned(),
    }))
}

fn start_line(input: &[u8]) -> IResult<&[u8], StartLine> {
    alt((
        map(request_line, StartLine::Request),
        map(status_line, StartLine::Response),
    ))(input)
}

fn headers(input: &[u8]) -> IResult<&[u8], HashMap<String, String>> {
    let (input, headers) = many0(header)(input)?;
    let (input, _) = tag("\r\n")(input)?;
    
    Ok((input, headers.into_iter().collect()))
}

fn http_message(input: &[u8]) -> IResult<&[u8], HttpMessage> {
    let (input, start_line) = start_line(input)?;
    let (input, headers) = headers(input)?;
    let (input, body) = if !input.is_empty() {
        let content_length = headers.get("Content-Length")
            .and_then(|len| len.parse::<usize>().ok())
            .unwrap_or(0);
        if content_length > 0 {
            let (input, body) = take_while(|_| true)(input)?;
            (input, Some(body.to_vec()))
        } else {
            (input, None)
        }
    } else {
        (input, None)
    };

    Ok((input, HttpMessage {
        start_line,
        headers,
        body,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let input = match fs::read(&args[1]) {
        Ok(contents) => contents,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };

    match http_message(&input) {
        Ok((remaining, message)) => {
            println!("Parsed HTTP message: {:#?}", message);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining unparsed", remaining.len());
            }
        }
        Err(e) => {
            eprintln!("Error parsing HTTP message: {}", e);
        }
    }
}