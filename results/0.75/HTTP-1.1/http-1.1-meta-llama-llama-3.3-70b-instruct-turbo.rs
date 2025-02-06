use nom::{
    IResult,
    bytes::complete::{tag, take_while1, tag_no_case},
    character::complete::{digit1, multispace0},
    combinator::{map_res},
    multi::{many0},
    sequence::{preceded, tuple},
};
use std::fs::File;
use std::io::Read;
use std::env;
use std::str;

const CRLF: &str = "\r\n";

#[derive(Debug, PartialEq)]
enum HttpMethod {
    OPTIONS,
    GET,
    HEAD,
    POST,
    PUT,
    DELETE,
    TRACE,
    CONNECT,
}

impl HttpMethod {
    fn parse(input: &str) -> IResult<&str, HttpMethod> {
        let (input, method) = tag_no_case("OPTIONS")(input)
            .or_else(|_: nom::Err<nom::error::Error<&str>>| tag_no_case("GET")(input))
            .or_else(|_: nom::Err<nom::error::Error<&str>>| tag_no_case("HEAD")(input))
            .or_else(|_: nom::Err<nom::error::Error<&str>>| tag_no_case("POST")(input))
            .or_else(|_: nom::Err<nom::error::Error<&str>>| tag_no_case("PUT")(input))
            .or_else(|_: nom::Err<nom::error::Error<&str>>| tag_no_case("DELETE")(input))
            .or_else(|_: nom::Err<nom::error::Error<&str>>| tag_no_case("TRACE")(input))
            .or_else(|_: nom::Err<nom::error::Error<&str>>| tag_no_case("CONNECT")(input))?;
        Ok((input, match method {
            "OPTIONS" => HttpMethod::OPTIONS,
            "GET" => HttpMethod::GET,
            "HEAD" => HttpMethod::HEAD,
            "POST" => HttpMethod::POST,
            "PUT" => HttpMethod::PUT,
            "DELETE" => HttpMethod::DELETE,
            "TRACE" => HttpMethod::TRACE,
            "CONNECT" => HttpMethod::CONNECT,
            _ => unreachable!(),
        }))
    }
}

fn http_version(input: &str) -> IResult<&str, &str> {
    tag_no_case("HTTP/1.1")(input)
}

fn request_uri(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c != ' ' && c != '\r' && c != '\n')(input)
}

fn request_line(input: &str) -> IResult<&str, (HttpMethod, &str, &str)> {
    tuple((HttpMethod::parse, request_uri, http_version))(input)
}

fn header_name(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c.is_alphanumeric() || c == '-' || c == '_')(input)
}

fn header_value(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c != '\r' && c != '\n')(input)
}

fn header(input: &str) -> IResult<&str, (&str, &str)> {
    preceded(multispace0, tuple((header_name, tag(": "), header_value)))(input)
}

fn headers(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
    many0(header)(input)
}

fn request(input: &str) -> IResult<&str, (HttpMethod, &str, &str, Vec<(&str, &str)>)> {
    let (input, (method, uri, version)) = request_line(input)?;
    let (input, headers) = headers(input)?;
    Ok((input, (method, uri, version, headers)))
}

fn status_code(input: &str) -> IResult<&str, u16> {
    map_res(digit1, |s: &str| s.parse::<u16>())(input)
}

fn reason_phrase(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c != '\r' && c != '\n')(input)
}

fn status_line(input: &str) -> IResult<&str, (u16, &str)> {
    tuple((status_code, reason_phrase))(input)
}

fn response(input: &str) -> IResult<&str, (u16, &str, Vec<(&str, &str)>)> {
    let (input, (code, phrase)) = status_line(input)?;
    let (input, headers) = headers(input)?;
    Ok((input, (code, phrase, headers)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }

    let mut file = File::open(&args[1]).unwrap();
    let mut input = Vec::new();
    file.read_to_end(&mut input).unwrap();
    let input_str = str::from_utf8(&input).unwrap();

    let (_remaining, request) = request(input_str).unwrap();
    let (_remaining, response) = response(input_str).unwrap();

    println!("{:?}", request);
    println!("{:?}", response);
}