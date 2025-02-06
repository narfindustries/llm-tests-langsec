use nom::{
    bytes::complete::{tag, take_while},
    character::complete::{char, digit1, space1},
    combinator::{map, map_res, opt},
    multi::many0,
    sequence::{preceded, terminated, tuple},
    IResult,
};
use std::{env, fs};

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<(String, String)>,
    body: Option<String>,
}

fn is_token_char(c: char) -> bool {
    !c.is_control() && !c.is_whitespace() && ",;:".chars().all(|x| x != c)
}

fn parse_method(input: &str) -> IResult<&str, String> {
    map_res(take_while(is_token_char), str::parse)(input)
}

fn parse_uri(input: &str) -> IResult<&str, String> {
    map_res(take_while(is_token_char), str::parse)(input)
}

fn parse_version(input: &str) -> IResult<&str, String> {
    map_res(preceded(tag("HTTP/"), digit1), |s: &str| Ok::<_, nom::error::Error<_>>(format!("HTTP/{}", s)))(input)
}

fn parse_header_value(input: &str) -> IResult<&str, String> {
    map_res(take_while(|c| c != '\r' && c != '\n'), str::parse)(input)
}

fn parse_header(input: &str) -> IResult<&str, (String, String)> {
    terminated(
        map(
            tuple((
                map_res(take_while(is_token_char), str::parse::<String>),
                preceded(tag(":"), preceded(space1, parse_header_value)),
            )),
            |(k, v)| (k, v),
        ),
        opt(tuple((char('\r'), char('\n')))),
    )(input)
}

fn parse_request(input: &str) -> IResult<&str, HttpRequest> {
    map(
        tuple((
            terminated(parse_method, space1),
            terminated(parse_uri, space1),
            terminated(parse_version, tag("\r\n")),
            many0(parse_header),
            opt(preceded(tag("\r\n"), map_res(take_while(|c| c != '\0'), str::parse::<String>))),
        )),
        |(method, uri, version, headers, body)| HttpRequest {
            method,
            uri,
            version,
            headers,
            body,
        },
    )(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err("Usage: <program> <file_path>".into());
    }
    
    let filename = &args[1];
    let data = fs::read_to_string(filename)?;
    match parse_request(&data) {
        Ok((remaining, request)) => {
            if !remaining.is_empty() {
                eprintln!("Warning: unconsumed input");
            }
            println!("{:?}", request);
        }
        Err(e) => {
            eprintln!("Error parsing HTTP request: {:?}", e);
        }
    }

    Ok(())
}