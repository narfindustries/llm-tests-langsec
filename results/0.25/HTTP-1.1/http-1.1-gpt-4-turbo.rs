use nom::{
    bytes::complete::{tag, take_until, take_while},
    character::complete::{char, digit1, line_ending, space1},
    combinator::{map_res, opt},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<(String, String)>,
    body: Option<String>,
}

fn is_token_char(c: char) -> bool {
    c.is_ascii() && !c.is_control() && !",;= \t()<>@:\\\"/[]?{}".contains(c)
}

fn parse_method(input: &str) -> IResult<&str, String> {
    map_res(take_while(is_token_char), str::parse)(input)
}

fn parse_uri(input: &str) -> IResult<&str, String> {
    map_res(take_until(" "), str::parse)(input)
}

fn parse_version(input: &str) -> IResult<&str, String> {
    preceded(tag("HTTP/"), map_res(digit1, str::parse))(input)
}

fn parse_header(input: &str) -> IResult<&str, (String, String)> {
    let (input, key) = map_res(take_while(is_token_char), str::parse)(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, _) = space1(input)?;
    let (input, value) = map_res(take_until("\r\n"), str::parse)(input)?;
    Ok((input, (key, value)))
}

fn parse_headers(input: &str) -> IResult<&str, Vec<(String, String)>> {
    separated_list0(line_ending, parse_header)(input)
}

fn parse_request(input: &str) -> IResult<&str, HttpRequest> {
    let (input, method) = terminated(parse_method, space1)(input)?;
    let (input, uri) = terminated(parse_uri, space1)(input)?;
    let (input, version) = terminated(parse_version, line_ending)(input)?;
    let (input, headers) = terminated(parse_headers, line_ending)(input)?;
    let (input, body) = opt(preceded(line_ending, map_res(take_until(""), str::parse)))(input)?;
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

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No input file specified"));
    }
    let mut file = File::open(&args[1])?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match parse_request(&contents) {
        Ok((_, request)) => println!("{:?}", request),
        Err(e) => println!("Failed to parse request: {:?}", e),
    }

    Ok(())
}