use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::complete::{char, digit1, line_ending, space1},
    combinator::{map_res, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
pub struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<(String, String)>,
    body: Option<String>,
}

#[derive(Debug)]
pub struct HttpResponse {
    version: String,
    status_code: u32,
    reason_phrase: String,
    headers: Vec<(String, String)>,
    body: Option<String>,
}

fn http_version(input: &str) -> IResult<&str, &str> {
    tag("HTTP/")(input)
}

fn request_line(input: &str) -> IResult<&str, (&str, &str, &str)> {
    tuple((
        take_until(" "),
        delimited(space1, take_until(" "), space1),
        http_version,
    ))(input)
}

fn header_line(input: &str) -> IResult<&str, (&str, &str)> {
    tuple((
        take_until(":"),
        delimited(tag(": "), take_until("\r\n"), line_ending),
    ))(input)
}

fn parse_request(input: &str) -> IResult<&str, HttpRequest> {
    let (input, (method, uri, version)) = request_line(input)?;
    let (input, headers) = many0(header_line)(input)?;
    let (_, body) = opt(preceded(line_ending, take_until("")))(input)?;
    Ok((
        "",
        HttpRequest {
            method: method.to_string(),
            uri: uri.to_string(),
            version: version.to_string(),
            headers: headers.into_iter().map(|(k, v)| (k.to_string(), v.to_string())).collect(),
            body: body.map(str::to_string),
        },
    ))
}

fn parse_response(input: &str) -> IResult<&str, HttpResponse> {
    let (input, version) = http_version(input)?;
    let (input, (status, reason_phrase)) = tuple((preceded(space1, digit1), preceded(space1, take_until("\r\n"))))(input)?;
    let (input, headers) = many0(header_line)(input)?;
    let (_, body) = opt(preceded(line_ending, take_until("")))(input)?;
    Ok((
        "",
        HttpResponse {
            version: version.to_string(),
            status_code: status.parse().unwrap(),
            reason_phrase: reason_phrase.to_string(),
            headers: headers.into_iter().map(|(k, v)| (k.to_string(), v.to_string())).collect(),
            body: body.map(str::to_string),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match parse_request(&contents) {
        Ok(("", request)) => println!("{:?}", request),
        Ok((remaining, _)) => println!("Incomplete parsing, remaining: {:?}", remaining),
        Err(e) => println!("Error during parsing: {:?}", e),
    }

    Ok(())
}