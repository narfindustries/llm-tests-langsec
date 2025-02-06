use nom::bytes::complete::{take_while, tag, take_until, take};
use nom::character::complete::{char, space0, digit1};
use nom::combinator::{opt, map_res};
use nom::sequence::{tuple, preceded, terminated, delimited};
use nom::multi::{many0, separated_list0};
use nom::IResult;
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<Header>,
}

#[derive(Debug)]
struct Header {
    name: String,
    value: String,
}

fn is_token_char(c: char) -> bool {
    c.is_alphanumeric() || "!#$%&'*+-.^_`|~".contains(c)
}

fn token(input: &str) -> IResult<&str, &str> {
    take_while(is_token_char)(input)
}

fn header_value(input: &str) -> IResult<&str, &str> {
    take_while(|c| c != '\r' && c != '\n')(input)
}

fn header(input: &str) -> IResult<&str, Header> {
    let (input, name) = token(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = header_value(input)?;
    let (input, _) = tuple((char('\r'), char('\n')))(input)?;

    Ok((input, Header {
        name: name.to_string(),
        value: value.to_string(),
    }))
}

fn request_line(input: &str) -> IResult<&str, (String, String, String)> {
    let method = token;
    let uri = take_until(" ");
    let version = preceded(tag("HTTP/"), take_until("\r\n"));
    let (input, (method, _, uri, _, version, _)) = tuple((method, space0, uri, space0, version, tuple((char('\r'), char('\n')))))(input)?;

    Ok((input, (method.to_string(), uri.to_string(), version.to_string())))
}

fn http_request(input: &str) -> IResult<&str, HttpRequest> {
    let (input, (method, uri, version)) = request_line(input)?;
    let (input, headers) = many0(header)(input)?;
    let (input, _) = tuple((char('\r'), char('\n')))(input)?;

    Ok((input, HttpRequest {
        method,
        uri,
        version,
        headers,
    }))
}

fn parse_http_request(input: &str) -> Result<HttpRequest, String> {
    match http_request(input) {
        Ok((_, req)) => Ok(req),
        Err(err) => Err(format!("Failed to parse HTTP request: {:?}", err)),
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;

    match parse_http_request(&buffer) {
        Ok(req) => println!("{:?}", req),
        Err(err) => eprintln!("{}", err),
    }

    Ok(())
}