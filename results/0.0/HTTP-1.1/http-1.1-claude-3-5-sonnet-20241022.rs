use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{digit1, space0, space1, crlf, alphanumeric1},
    sequence::{tuple, delimited},
    multi::{many0, many1},
    branch::alt,
    combinator::{map, opt, recognize},
};

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
    !matches!(c, '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '[' | ']' | '?' | '=' | '{' | '}' | ' ' | '\t')
}

fn parse_token(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while1(|c| is_token_char(c as char))(input)
}

fn parse_header_value(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_until("\r\n")(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, (name, _, _, value, _)) = tuple((
        parse_token,
        tag(":"),
        space0,
        parse_header_value,
        crlf
    ))(input)?;

    Ok((input, (
        String::from_utf8_lossy(name).to_string(),
        String::from_utf8_lossy(value).trim().to_string()
    )))
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    let (input, (method, _, uri, _, version, _)) = tuple((
        alphanumeric1,
        space1,
        take_until(" "),
        space1,
        take_until("\r\n"),
        crlf
    ))(input)?;

    Ok((input, (
        String::from_utf8_lossy(method).to_string(),
        String::from_utf8_lossy(uri).to_string(),
        String::from_utf8_lossy(version).to_string()
    )))
}

fn parse_status_line(input: &[u8]) -> IResult<&[u8], (String, u16, String)> {
    let (input, (version, _, status_code, _, status_text, _)) = tuple((
        take_until(" "),
        space1,
        digit1,
        space1,
        take_until("\r\n"),
        crlf
    ))(input)?;

    Ok((input, (
        String::from_utf8_lossy(version).to_string(),
        String::from_utf8_lossy(status_code).parse().unwrap(),
        String::from_utf8_lossy(status_text).to_string()
    )))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<(String, String)>> {
    let (input, headers) = many0(parse_header)(input)?;
    let (input, _) = crlf(input)?;
    Ok((input, headers))
}

fn parse_body(input: &[u8]) -> IResult<&[u8], Option<Vec<u8>>> {
    if input.is_empty() {
        Ok((input, None))
    } else {
        Ok((&[], Some(input.to_vec())))
    }
}

fn parse_http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, uri, version)) = parse_request_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, body) = parse_body(input)?;

    Ok((input, HttpRequest {
        method,
        uri,
        version,
        headers,
        body,
    }))
}

fn parse_http_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    let (input, (version, status_code, status_text)) = parse_status_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, body) = parse_body(input)?;

    Ok((input, HttpResponse {
        version,
        status_code,
        status_text,
        headers,
        body,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut contents = Vec::new();
    file.read_to_end(&mut contents).expect("Failed to read file");

    match parse_http_request(&contents) {
        Ok((_, request)) => println!("Parsed HTTP Request: {:?}", request),
        Err(_) => {
            match parse_http_response(&contents) {
                Ok((_, response)) => println!("Parsed HTTP Response: {:?}", response),
                Err(e) => eprintln!("Failed to parse HTTP message: {:?}", e),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_request() {
        let input = b"GET /index.html HTTP/1.1\r\n\
                     Host: www.example.com\r\n\
                     User-Agent: Mozilla/5.0\r\n\r\n";
        
        let result = parse_http_request(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_response() {
        let input = b"HTTP/1.1 200 OK\r\n\
                     Content-Type: text/html\r\n\
                     Content-Length: 0\r\n\r\n";
        
        let result = parse_http_response(input);
        assert!(result.is_ok());
    }
}