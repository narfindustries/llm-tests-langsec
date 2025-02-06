use nom::{
    bytes::complete::{tag, take_until, take_while},
    character::complete::{char, digit1, space1},
    combinator::{map_res, opt},
    multi::separated_list0,
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<(String, String)>,
    body: Option<String>,
}

fn is_token_char(c: char) -> bool {
    c.is_alphanumeric() || "-!#$%&'*+.^_`|~".contains(c)
}

fn token(input: &str) -> IResult<&str, &str> {
    take_while(is_token_char)(input)
}

fn header_value(input: &str) -> IResult<&str, &str> {
    take_until("\r\n")(input)
}

fn header(input: &str) -> IResult<&str, (String, String)> {
    let (input, (name, _, value, _)) = tuple((
        token,
        char(':'),
        preceded(space1, header_value),
        tag("\r\n"),
    ))(input)?;
    Ok((input, (name.to_string(), value.to_string())))
}

fn headers(input: &str) -> IResult<&str, Vec<(String, String)>> {
    separated_list0(tag("\r\n"), header)(input)
}

fn request_line(input: &str) -> IResult<&str, (String, String, String)> {
    let (input, (method, _, uri, _, version, _)) = tuple((
        token,
        space1,
        take_until(" "),
        space1,
        take_until("\r\n"),
        tag("\r\n"),
    ))(input)?;
    Ok((input, (method.to_string(), uri.to_string(), version.to_string())))
}

fn http_request(input: &str) -> IResult<&str, HttpRequest> {
    let (input, (method, uri, version)) = request_line(input)?;
    let (input, headers) = headers(input)?;
    let (input, _) = tag("\r\n")(input)?;
    let (input, body) = opt(take_until(""))(input)?;
    Ok((
        input,
        HttpRequest {
            method,
            uri,
            version,
            headers,
            body: body.map(|s| s.to_string()),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;

    match http_request(&buffer) {
        Ok((_, request)) => println!("{:?}", request),
        Err(e) => eprintln!("Failed to parse HTTP request: {:?}", e),
    }

    Ok(())
}