use nom::{
    bytes::complete::{tag, take_while, take_until},
    character::complete::{alpha1, char, digit1, line_ending, space0, space1},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, separated_list0},
    number::complete::double,
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::fs;
use std::path::Path;

#[derive(Debug, PartialEq)]
enum Method {
    Get,
    Post,
    // Add other methods as needed
}

#[derive(Debug, PartialEq)]
struct RequestLine {
    method: Method,
    path: String,
    version: String,
}

#[derive(Debug, PartialEq)]
struct Header {
    name: String,
    value: String,
}

#[derive(Debug, PartialEq)]
struct Request {
    request_line: RequestLine,
    headers: Vec<Header>,
    body: Vec<u8>,
}

fn method(input: &[u8]) -> IResult<&[u8], Method> {
    let (input, method) = alpha1(input)?;
    match method {
        b"GET" => Ok((input, Method::Get)),
        b"POST" => Ok((input, Method::Post)),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn request_line(input: &[u8]) -> IResult<&[u8], RequestLine> {
    let (input, (method, path, version)) = tuple((
        method,
        delimited(space1, take_until(b" HTTP/"), space0),
        recognize(tuple((tag(b"HTTP/"), digit1, opt(char(b'.'))))),
    ))(input)?;
    Ok((
        input,
        RequestLine {
            method: method,
            path: String::from_utf8_lossy(path).to_string(),
            version: String::from_utf8_lossy(version).to_string(),
        },
    ))
}

fn header(input: &[u8]) -> IResult<&[u8], Header> {
    let (input, (name, value)) = separated_pair(
        take_until(b":"),
        tag(b": "),
        take_until(line_ending),
    )(input)?;
    Ok((
        input,
        Header {
            name: String::from_utf8_lossy(name).trim().to_string(),
            value: String::from_utf8_lossy(value).trim().to_string(),
        },
    ))
}

fn headers(input: &[u8]) -> IResult<&[u8], Vec<Header>> {
    many0(terminated(header, line_ending))(input)
}


fn request(input: &[u8]) -> IResult<&[u8], Request> {
    let (input, (request_line, headers, body)) = tuple((
        request_line,
        headers,
        take_while(|c| c != 0), //Simple body parser.  Improve as needed for your use case.
    ))(input)?;
    Ok((
        input,
        Request {
            request_line,
            headers,
            body: body.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let contents = fs::read(path).expect("Failed to read file");

    match request(&contents) {
        Ok((_, req)) => println!("{:#?}", req),
        Err(e) => println!("Error parsing request: {:?}", e),
    }
}

