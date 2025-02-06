use nom::{
    bytes::complete::{tag, take_while, take_until},
    character::complete::{alpha1, alphanumeric1, line_ending, space0, space1},
    combinator::{map, map_res, opt, recognize},
    multi::separated_list0,
    sequence::{pair, preceded, separated_pair, tuple},
    IResult,
};
use std::fs;
use std::path::Path;
use std::str::FromStr;

#[derive(Debug)]
struct HttpRequest {
    method: String,
    path: String,
    http_version: String,
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}

#[derive(Debug)]
struct HttpResponse {
    http_version: String,
    status_code: u16,
    status_text: String,
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}


fn http_header(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let parse_header_name = map_res(
        take_while(|c: u8| c.is_ascii_alphanumeric() || c == b'-' || c == b'_'),
        |bytes: &[u8]| String::from_utf8(bytes.to_vec()),
    );
    let parse_header_value = map_res(
        take_until("\r\n"),
        |bytes: &[u8]| String::from_utf8(bytes.to_vec()),
    );

    separated_pair(parse_header_name, space1, parse_header_value)(input)
}

fn http_headers(input: &[u8]) -> IResult<&[u8], Vec<(String, String)>> {
    separated_list0(line_ending, http_header)(input)
}

fn http_start_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    let parse_method = map_res(alpha1, |bytes: &[u8]| String::from_utf8(bytes.to_vec()));
    let parse_path = map_res(take_until(" HTTP/"), |bytes: &[u8]| String::from_utf8(bytes.to_vec()));
    let parse_version = map_res(
        recognize(pair(tag("HTTP/"), alphanumeric1)),
        |bytes: &[u8]| String::from_utf8(bytes.to_vec()),
    );

    tuple((parse_method, parse_path, parse_version))(input)
}

fn http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, path, version)) = preceded(space0, http_start_line)(input)?;
    let (input, headers) = preceded(line_ending, http_headers)(input)?;
    let (input, body) = take_until("\r\n")(input)?;

    Ok((
        input,
        HttpRequest {
            method,
            path,
            http_version: version,
            headers,
            body: body.to_vec(),
        },
    ))
}

fn http_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    let parse_version = map_res(
        recognize(pair(tag("HTTP/"), alphanumeric1)),
        |bytes: &[u8]| String::from_utf8(bytes.to_vec()),
    );
    let parse_status_code = map_res(
        recognize(pair(space1, alphanumeric1)),
        |bytes: &[u8]| u16::from_str(std::str::from_utf8(bytes).unwrap()),
    );
    let parse_status_text = map_res(take_until("\r\n"), |bytes: &[u8]| String::from_utf8(bytes.to_vec()));

    let (input, (version, status_code, status_text)) = tuple((parse_version, parse_status_code, parse_status_text))(input)?;
    let (input, headers) = preceded(line_ending, http_headers)(input)?;
    let (input, body) = take_until("\r\n")(input)?;

    Ok((
        input,
        HttpResponse {
            http_version: version,
            status_code,
            status_text,
            headers,
            body: body.to_vec(),
        },
    ))
}


fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let contents = fs::read(path).expect("Failed to read file");

    match http_request(&contents) {
        Ok((_, request)) => println!("Request: {:?}", request),
        Err(e) => println!("Error parsing request: {:?}", e),
    }

    match http_response(&contents) {
        Ok((_, response)) => println!("Response: {:?}", response),
        Err(e) => println!("Error parsing response: {:?}", e),
    }
}
