use nom::{
    bytes::complete::{tag, take_while1},
    character::complete::{alpha1, alphanumeric1, char, space0},
    combinator::{opt, recognize},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::fs;
use std::path::Path;

#[derive(Debug, PartialEq)]
enum HttpMethod {
    Get,
    Post,
    Put,
    Delete,
    Other(String),
}

#[derive(Debug, PartialEq)]
struct HttpRequest {
    method: HttpMethod,
    path: String,
    http_version: String,
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct HttpResponse {
    http_version: String,
    status_code: u16,
    status_text: String,
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}

fn http_method(input: &[u8]) -> IResult<&[u8], HttpMethod> {
    let (input, method) = recognize(pair(alpha1, opt(alphanumeric1)))(input)?;
    match method {
        b"GET" => Ok((input, HttpMethod::Get)),
        b"POST" => Ok((input, HttpMethod::Post)),
        b"PUT" => Ok((input, HttpMethod::Put)),
        b"DELETE" => Ok((input, HttpMethod::Delete)),
        _ => Ok((input, HttpMethod::Other(String::from_utf8_lossy(method).to_string()))),
    }
}

fn http_version(input: &[u8]) -> IResult<&[u8], String> {
    let (input, version) = recognize(tuple((tag(b"HTTP/"), alphanumeric1, char(b'.'), alphanumeric1)))(input)?;
    Ok((input, String::from_utf8_lossy(version).to_string()))
}

fn header_field(input: &[u8]) -> IResult<&[u8], (String, String)> {
    let (input, field) = terminated(take_while1(|c: u8| c != b':' && c != b'\r'), char(b':'))(input)?;
    let (input, value) = delimited(space0, take_while1(|c: u8| c != b'\r'), space0)(input)?;
    Ok((
        input,
        (
            String::from_utf8_lossy(field).trim().to_string(),
            String::from_utf8_lossy(value).trim().to_string(),
        ),
    ))
}

fn headers(input: &[u8]) -> IResult<&[u8], Vec<(String, String)>> {
    separated_list0(tag(b"\r\n"), header_field)(input)
}

fn request_line(input: &[u8]) -> IResult<&[u8], (HttpMethod, String, String)> {
    let (input, (method, path, version)) = tuple((http_method, take_while1(|c: u8| c != b' '), http_version))(input)?;
    Ok((input, (method, String::from_utf8_lossy(path).to_string(), version)))
}

fn http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, (method, path, version)) = preceded(space0, request_line)(input)?;
    let (input, headers) = preceded(tag(b"\r\n"), headers)(input)?;
    let (input, body) = if input.is_empty() {
        (input, &[])
    } else {
        let (i, b) = take_while1(|_| true)(input);
        (i, b)
    };
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

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let contents = fs::read(path).expect("Something went wrong reading the file");

    match http_request(&contents) {
        Ok((leftover, request)) => {
            println!("Parsed Request:\n{:#?}", request);
            if !leftover.is_empty() {
                println!("Leftover data: {:?}", leftover);
            }
        }
        Err(e) => println!("Error parsing HTTP request: {}", e),
    }
}
