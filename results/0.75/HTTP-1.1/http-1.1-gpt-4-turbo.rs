use nom::{
    bytes::complete::{tag, take_until, take_while},
    character::complete::{char, space1},
    combinator::{opt, map_res},
    multi::separated_list1,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::str;

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<(String, String)>,
    body: Option<String>,
}

fn is_space(c: char) -> bool {
    c.is_whitespace()
}

fn not_space(input: &str) -> IResult<&str, &str> {
    take_while(|c: char| !c.is_whitespace())(input)
}

fn parse_request_line(input: &str) -> IResult<&str, (&str, &str, &str)> {
    tuple((
        not_space,
        delimited(space1, not_space, space1),
        delimited(tag("HTTP/"), not_space, char('\r')),
    ))(input)
}

fn parse_header(input: &str) -> IResult<&str, (&str, &str)> {
    pair(
        terminated(not_space, tag(":")),
        delimited(space1, take_until("\r\n"), tag("\r\n"))
    )(input)
}

fn parse_headers(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
    separated_list1(tag("\r\n"), parse_header)(input)
}

fn parse_http_request(input: &str) -> IResult<&str, HttpRequest> {
    let (input, (method, uri, version)) = terminated(parse_request_line, tag("\n"))(input)?;
    let (input, headers) = terminated(parse_headers, tag("\n"))(input)?;
    let (input, body) = opt(preceded(tag("\n"), take_while(|c| c != '\0')))(input)?;

    let headers = headers
        .into_iter()
        .map(|(key, value)| (key.to_string(), value.to_string()))
        .collect();

    let request = HttpRequest {
        method: method.to_string(),
        uri: uri.to_string(),
        version: version.to_string(),
        headers,
        body: body.map(|s| s.to_string()),
    };

    Ok((input, request))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Please provide one argument which is the file path",
        ));
    }

    let mut file = File::open(&args[1])?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;

    let input = match str::from_utf8(&contents) {
        Ok(v) => v,
        Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidData, e)),
    };

    match parse_http_request(input) {
        Ok((_rest, request)) => {
            println!("{:?}", request);
        }
        Err(e) => println!("Failed to parse HTTP request: {:?}", e),
    }

    Ok(())
}