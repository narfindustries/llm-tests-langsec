use nom::{
    bytes::complete::{tag, take_while_m_n},
    character::complete::{char, digit1, multispace0, multispace1},
    combinator::{map, map_res, opt},
    error::{ErrorKind, ParseError},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::{
    fs::File,
    io::{BufReader, Read},
    str,
};

#[derive(Debug, PartialEq)]
enum HttpMethod {
    GET,
    HEAD,
    POST,
    PUT,
    DELETE,
    CONNECT,
    OPTIONS,
    TRACE,
    PATCH,
}

impl HttpMethod {
    fn parse(input: &[u8]) -> IResult<&[u8], HttpMethod> {
        alt((tag("GET"), tag("HEAD"), tag("POST"), tag("PUT"), tag("DELETE"), tag("CONNECT"), tag("OPTIONS"), tag("TRACE"), tag("PATCH")))(input)
            .map(|(input, method)| {
                let method = match method {
                    b"GET" => HttpMethod::GET,
                    b"HEAD" => HttpMethod::HEAD,
                    b"POST" => HttpMethod::POST,
                    b"PUT" => HttpMethod::PUT,
                    b"DELETE" => HttpMethod::DELETE,
                    b"CONNECT" => HttpMethod::CONNECT,
                    b"OPTIONS" => HttpMethod::OPTIONS,
                    b"TRACE" => HttpMethod::TRACE,
                    b"PATCH" => HttpMethod::PATCH,
                    _ => unreachable!(),
                };
                (input, method)
            })
    }
}

#[derive(Debug, PartialEq)]
enum HttpVersion {
    HTTP1_0,
    HTTP1_1,
}

impl HttpVersion {
    fn parse(input: &[u8]) -> IResult<&[u8], HttpVersion> {
        alt((tag("HTTP/1.0"), tag("HTTP/1.1")))(input)
            .map(|(input, version)| {
                let version = match version {
                    b"HTTP/1.0" => HttpVersion::HTTP1_0,
                    b"HTTP/1.1" => HttpVersion::HTTP1_1,
                    _ => unreachable!(),
                };
                (input, version)
            })
    }
}

#[derive(Debug, PartialEq)]
struct HttpStatus {
    code: u16,
    reason: String,
}

impl HttpStatus {
    fn parse(input: &[u8]) -> IResult<&[u8], HttpStatus> {
        let (input, code) = map_res(digit1, |s: &[u8]| str::from_utf8(s).unwrap().parse::<u16>())(input)?;
        let (input, _) = char(' ')(input)?;
        let (input, reason) = take_while_m_n(1, 1024, |c| c != b'\r' && c != b'\n')(input)?;
        let reason = String::from_utf8_lossy(reason).into_owned();
        Ok((input, HttpStatus { code, reason }))
    }
}

#[derive(Debug, PartialEq)]
struct HttpRequest {
    method: HttpMethod,
    request_uri: String,
    version: HttpVersion,
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}

impl HttpRequest {
    fn parse(input: &[u8]) -> IResult<&[u8], HttpRequest> {
        let (input, method) = HttpMethod::parse(input)?;
        let (input, _) = multispace1(input)?;
        let (input, request_uri) = take_while_m_n(1, 1024, |c| c != b' ')(input)?;
        let request_uri = String::from_utf8_lossy(request_uri).into_owned();
        let (input, _) = multispace1(input)?;
        let (input, version) = HttpVersion::parse(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = char('\r')(input)?;
        let (input, _) = char('\n')(input)?;
        let (input, headers) = many0(
            tuple((
                take_while_m_n(1, 1024, |c| c != b':'),
                char(':'),
                multispace0,
                take_while_m_n(0, 1024, |c| c != b'\r' && c != b'\n'),
                char('\r'),
                char('\n'),
            ))
        )(input)?;
        let headers: Vec<(String, String)> = headers
            .into_iter()
            .map(|(name, _, _, value, _, _)| {
                (
                    String::from_utf8_lossy(name).into_owned(),
                    String::from_utf8_lossy(value).into_owned(),
                )
            })
            .collect();
        let (input, body) = take_while_m_n(0, 1024 * 1024, |c| c != b'\r' && c != b'\n')(input)?;
        Ok((input, HttpRequest { method, request_uri, version, headers, body }))
    }
}

#[derive(Debug, PartialEq)]
struct HttpResponse {
    version: HttpVersion,
    status: HttpStatus,
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}

impl HttpResponse {
    fn parse(input: &[u8]) -> IResult<&[u8], HttpResponse> {
        let (input, version) = HttpVersion::parse(input)?;
        let (input, _) = multispace1(input)?;
        let (input, status) = HttpStatus::parse(input)?;
        let (input, _) = multispace0(input)?;
        let (input, _) = char('\r')(input)?;
        let (input, _) = char('\n')(input)?;
        let (input, headers) = many0(
            tuple((
                take_while_m_n(1, 1024, |c| c != b':'),
                char(':'),
                multispace0,
                take_while_m_n(0, 1024, |c| c != b'\r' && c != b'\n'),
                char('\r'),
                char('\n'),
            ))
        )(input)?;
        let headers: Vec<(String, String)> = headers
            .into_iter()
            .map(|(name, _, _, value, _, _)| {
                (
                    String::from_utf8_lossy(name).into_owned(),
                    String::from_utf8_lossy(value).into_owned(),
                )
            })
            .collect();
        let (input, body) = take_while_m_n(0, 1024 * 1024, |c| c != b'\r' && c != b'\n')(input)?;
        Ok((input, HttpResponse { version, status, headers, body }))
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    match HttpRequest::parse(&input) {
        Ok((_, request)) => println!("{:?}", request),
        Err(err) => println!("Error: {:?}", err),
    }
    match HttpResponse::parse(&input) {
        Ok((_, response)) => println!("{:?}", response),
        Err(err) => println!("Error: {:?}", err),
    }
}