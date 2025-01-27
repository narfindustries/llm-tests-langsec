use nom::{
    bytes::complete::{tag, take_until, take_while},
    character::complete::{char, digit1, space1},
    combinator::{map_res, opt},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use std::{env, fs::File, io::Read, str::from_utf8};

#[derive(Debug)]
struct HttpRequest {
    method: String,
    uri: String,
    version: String,
    headers: Vec<(String, String)>,
    body: Option<String>,
}

fn is_token_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || b"!#$%&'*+-.^_`|~".contains(&(c as u8))
}

fn parse_method(input: &str) -> IResult<&str, String> {
    map_res(take_while(is_token_char), from_utf8)(input).map(|(next_input, res)| {
        (next_input, res.to_string())
    })
}

fn parse_uri(input: &str) -> IResult<&str, String> {
    map_res(take_until(" "), from_utf8)(input).map(|(next_input, res)| {
        (next_input, res.to_string())
    })
}

fn parse_version(input: &str) -> IResult<&str, String> {
    map_res(
        delimited(tag("HTTP/"), take_until("\r\n"), char('\r')),
        from_utf8,
    )(input)
    .map(|(next_input, res)| (next_input, res.to_string()))
}

fn parse_header(input: &str) -> IResult<&str, (String, String)> {
    let (input, key) = map_res(take_until(":"), from_utf8)(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space1(input)?;
    let (input, value) = map_res(take_until("\r\n"), from_utf8)(input)?;
    let (input, _) = tag("\r\n")(input)?;
    Ok((input, (key.to_string(), value.to_string())))
}

fn parse_headers(input: &str) -> IResult<&str, Vec<(String, String)>> {
    many0(parse_header)(input)
}

fn parse_body(input: &str) -> IResult<&str, Option<String>> {
    opt(map_res(take_until(""), from_utf8))(input).map(|(next_input, res)| {
        (next_input, res.map(|s| s.to_string()))
    })
}

fn parse_request(input: &str) -> IResult<&str, HttpRequest> {
    let (input, method) = terminated(parse_method, space1)(input)?;
    let (input, uri) = terminated(parse_uri, space1)(input)?;
    let (input, version) = terminated(parse_version, tag("\r\n"))(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, body) = parse_body(input)?;
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

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut contents = Vec::new();
    file.read_to_end(&mut contents).expect("Failed to read file");
    let contents_str = from_utf8(&contents).expect("Failed to convert to UTF-8");

    match parse_request(contents_str) {
        Ok((_, req)) => println!("{:#?}", req),
        Err(e) => eprintln!("Failed to parse HTTP request: {:?}", e),
    }
}