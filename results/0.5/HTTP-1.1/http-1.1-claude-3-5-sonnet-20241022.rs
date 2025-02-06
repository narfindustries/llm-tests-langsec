use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::space1,
    combinator::map,
    multi::many0,
    IResult,
};
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct HttpRequest {
    method: Method,
    uri: String,
    version: String,
    headers: HashMap<String, String>,
    body: Option<Vec<u8>>,
}

#[derive(Debug)]
enum Method {
    GET,
    POST,
    PUT,
    DELETE,
    HEAD,
    OPTIONS,
    TRACE,
    CONNECT,
}

fn is_token_char(c: u8) -> bool {
    c.is_ascii_alphanumeric() || b"!#$%&'*+-.^_`|~".contains(&c)
}

fn parse_method(input: &[u8]) -> IResult<&[u8], Method> {
    alt((
        map(tag("GET"), |_| Method::GET),
        map(tag("POST"), |_| Method::POST),
        map(tag("PUT"), |_| Method::PUT),
        map(tag("DELETE"), |_| Method::DELETE),
        map(tag("HEAD"), |_| Method::HEAD),
        map(tag("OPTIONS"), |_| Method::OPTIONS),
        map(tag("TRACE"), |_| Method::TRACE),
        map(tag("CONNECT"), |_| Method::CONNECT),
    ))(input)
}

fn parse_uri(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c| c != b' '),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(|c| c != b'\r'),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_header_name(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_while1(is_token_char),
        |s: &[u8]| String::from_utf8_lossy(s).to_string(),
    )(input)
}

fn parse_header_value(input: &[u8]) -> IResult<&[u8], String> {
    map(
        take_until("\r\n"),
        |s: &[u8]| String::from_utf8_lossy(s).trim().to_string(),
    )(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, name) = parse_header_name(input)?;
    let (input, _) = tag(": ")(input)?;
    let (input, value) = parse_header_value(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((input, (name, value)))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], HashMap<String, String>> {
    let (input, headers) = many0(parse_header)(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((input, headers.into_iter().collect()))
}

fn parse_body(input: &[u8]) -> IResult<&[u8], Option<Vec<u8>>> {
    if input.is_empty() {
        Ok((input, None))
    } else {
        Ok((&[], Some(input.to_vec())))
    }
}

fn parse_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, method) = parse_method(input)?;
    let (input, _) = space1(input)?;
    let (input, uri) = parse_uri(input)?;
    let (input, _) = space1(input)?;
    let (input, version) = parse_version(input)?;
    let (input, _) = tag("\r\n")(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, body) = parse_body(input)?;

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

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;

    match parse_request(&contents) {
        Ok((remaining, request)) => {
            println!("Parsed request: {:?}", request);
            if !remaining.is_empty() {
                println!("Remaining unparsed data: {:?}", remaining);
            }
        }
        Err(e) => eprintln!("Failed to parse request: {:?}", e),
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_request() {
        let input = b"GET /index.html HTTP/1.1\r\nHost: example.com\r\nUser-Agent: test\r\n\r\n";
        let (remaining, request) = parse_request(input).unwrap();
        assert!(remaining.is_empty());
        assert!(matches!(request.method, Method::GET));
        assert_eq!(request.uri, "/index.html");
        assert_eq!(request.version, "HTTP/1.1");
        assert_eq!(request.headers.get("Host").unwrap(), "example.com");
        assert_eq!(request.headers.get("User-Agent").unwrap(), "test");
    }

    #[test]
    fn test_parse_request_with_body() {
        let input = b"POST /submit HTTP/1.1\r\nContent-Length: 11\r\n\r\nHello World";
        let (remaining, request) = parse_request(input).unwrap();
        assert!(remaining.is_empty());
        assert!(matches!(request.method, Method::POST));
        assert_eq!(request.uri, "/submit");
        assert_eq!(request.version, "HTTP/1.1");
        assert_eq!(
            request.body.unwrap(),
            b"Hello World".to_vec()
        );
    }
}