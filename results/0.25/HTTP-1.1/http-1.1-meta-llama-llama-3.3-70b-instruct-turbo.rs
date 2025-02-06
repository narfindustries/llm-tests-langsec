use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, digit1, multispace0, multispace1},
    combinator::{map_res, recognize},
    multi::{many0},
    sequence::{preceded, separated_pair, tuple},
    IResult,
};
use std::{
    collections::HashMap,
    env,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

#[derive(Debug, PartialEq)]
enum HttpMessage {
    Request { method: String, request_uri: String, http_version: String, headers: HashMap<String, String> },
    Response { status_code: u16, reason_phrase: String, headers: HashMap<String, String> },
}

fn http_version(input: &str) -> IResult<&str, &str> {
    recognize(tuple((tag("HTTP/"), digit1, char('.'), digit1)))(input)
}

fn method(input: &str) -> IResult<&str, &str> {
    alt((tag("GET"), tag("HEAD"), tag("POST"), tag("PUT"), tag("DELETE"), tag("CONNECT"), tag("OPTIONS")))(input)
}

fn request_uri(input: &str) -> IResult<&str, &str> {
    recognize(take_while1(|c| c != ' '))(input)
}

fn request_line(input: &str) -> IResult<&str, (&str, &str, &str, &str)> {
    tuple((method, multispace1, request_uri, multispace1, http_version))(input)
}

fn header_name(input: &str) -> IResult<&str, &str> {
    recognize(take_while1(|c| c != ':'))(input)
}

fn header_value(input: &str) -> IResult<&str, &str> {
    recognize(preceded(multispace1, take_while1(|c| c != '\r' && c != '\n')))(input)
}

fn header(input: &str) -> IResult<&str, (&str, &str)> {
    separated_pair(header_name, char(':'), header_value)(input)
}

fn headers(input: &str) -> IResult<&str, HashMap<String, String>> {
    let mut headers = HashMap::new();
    let (input, headers_list) = many0(header)(input)?;
    for (name, value) in headers_list {
        headers.insert(name.to_string(), value.to_string());
    }
    Ok((input, headers))
}

fn request(input: &str) -> IResult<&str, HttpMessage> {
    let (input, (method, request_uri, _, http_version, headers)) = tuple((method, multispace1, request_uri, multispace1, http_version, multispace0, headers))(input)?;
    Ok((input, HttpMessage::Request { method: method.to_string(), request_uri: request_uri.to_string(), http_version: http_version.to_string(), headers }))
}

fn status_code(input: &str) -> IResult<&str, u16> {
    map_res(recognize(digit1), |s: &str| s.parse::<u16>())(input)
}

fn reason_phrase(input: &str) -> IResult<&str, &str> {
    recognize(take_while1(|c| c != '\r' && c != '\n'))(input)
}

fn status_line(input: &str) -> IResult<&str, (u16, &str)> {
    tuple((http_version, multispace1, status_code, multispace1, reason_phrase))(input)
}

fn response(input: &str) -> IResult<&str, HttpMessage> {
    let (input, (_, status_code, reason_phrase, headers)) = tuple((http_version, multispace1, status_code, multispace1, reason_phrase, multispace0, headers))(input)?;
    Ok((input, HttpMessage::Response { status_code, reason_phrase: reason_phrase.to_string(), headers }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let path = Path::new(input_file);
    let file = match File::open(path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };
    let mut reader = BufReader::new(file);
    let mut input = String::new();
    match reader.read_to_string(&mut input) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    }
    let result = alt((request, response))(&input);
    match result {
        Ok((_, output)) => println!("{:?}", output),
        Err(err) => eprintln!("Error parsing input: {:?}", err),
    }
}