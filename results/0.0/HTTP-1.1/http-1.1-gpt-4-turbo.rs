use nom::{
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, digit1, space1},
    combinator::{map_res, opt},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::str::{self, FromStr};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut contents = Vec::new();
    file.read_to_end(&mut contents).expect("Failed to read file");

    match parse_http_request(&contents) {
        Ok((_, request)) => println!("{:?}", request),
        Err(e) => eprintln!("Failed to parse HTTP request: {:?}", e),
    }
}

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<(String, String)>,
    body: Option<Vec<u8>>,
}

fn parse_http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, uri, version)) = parse_request_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, body) = opt(take_while(|_| true))(input)?;

    Ok((
        input,
        HttpRequest {
            method,
            uri,
            version,
            headers,
            body: body.map(|b| b.to_vec()),
        },
    ))
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    let (input, method) = map_res(delimited(space1, alpha1, space1), str::from_utf8)(input)?;
    let (input, uri) = map_res(delimited(space1, take_while(|c| c != b' '), space1), str::from_utf8)(input)?;
    let (input, version) = map_res(delimited(tag("HTTP/"), take_while(|c| c != b'\r'), tag("\r\n")), str::from_utf8)(input)?;

    Ok((input, (method.to_string(), uri.to_string(), version.to_string())))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<(String, String)>> {
    separated_list0(tag("\r\n"), parse_header)(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, key) = map_res(take_while(|c| c != b':'), str::from_utf8)(input)?;
    let (input, value) = map_res(preceded(tag(": "), take_while(|c| c != b'\r')), str::from_utf8)(input)?;
    let (input, _) = tag("\r\n")(input)?;

    Ok((input, (key.trim().to_string(), value.trim().to_string())))
}