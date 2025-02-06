use nom::{
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, space1},
    combinator::opt,
    multi::separated_list0,
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, Read};
use std::str;

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: HashMap<String, String>,
    body: Option<String>,
}

fn is_token_char(c: char) -> bool {
    c.is_alphanumeric() || "!#$%&'*+-.^_`|~".contains(c)
}

fn parse_request_line(input: &str) -> IResult<&str, (&str, &str, &str)> {
    tuple((
        alpha1,
        delimited(space1, take_while(|c: char| !c.is_whitespace()), space1),
        preceded(tag("HTTP/"), take_while(|c: char| c != '\r')),
    ))(input)
}

fn parse_header(input: &str) -> IResult<&str, (&str, &str)> {
    pair(
        take_while(is_token_char),
        preceded(tuple((tag(":"), space1)), take_while(|c| c != '\r')),
    )(input)
}

fn parse_request(input: &str) -> IResult<&str, HttpRequest> {
    let (input, (method, uri, version)) = parse_request_line(input)?;
    let (input, headers) = separated_list0(tag("\r\n"), parse_header)(input)?;
    let (input, _) = tag("\r\n\r\n")(input)?;
    let body = opt(take_while(|_| true))(input)?;

    let header_map = headers
        .into_iter()
        .map(|(k, v)| (k.to_string(), v.trim().to_string()))
        .collect();

    Ok((
        "",
        HttpRequest {
            method: method.to_string(),
            uri: uri.to_string(),
            version: version.to_string(),
            headers: header_map,
            body: body.map(|s| s.to_string()),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Usage: http_parser <file>",
        ));
    }

    let mut file = File::open(&args[1])?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;

    let contents_str = str::from_utf8(&contents).expect("File content is not valid UTF-8");

    match parse_request(contents_str) {
        Ok((_, request)) => {
            println!("{:#?}", request);
        }
        Err(e) => {
            eprintln!("Failed to parse HTTP request: {:?}", e);
        }
    }

    Ok(())
}