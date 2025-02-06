use nom::{
    bytes::complete::{tag, take_while},
    character::complete::{digit1, space1},
    combinator::{map_res, opt},
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
    body: Option<Vec<u8>>,
}

fn parse_http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    let (input, method) = map_res(take_while(|c: u8| c != b' '), str::from_utf8)(input)?;
    let (input, _) = space1(input)?;
    let (input, uri) = map_res(take_while(|c: u8| c != b' '), str::from_utf8)(input)?;
    let (input, _) = space1(input)?;
    let (input, version) = map_res(delimited(tag("HTTP/"), digit1, tag("\r\n")), str::from_utf8)(input)?;

    let (input, headers) = separated_list0(
        tag("\r\n"),
        map_res(
            tuple((
                map_res(take_while(|c: u8| c != b':'), str::from_utf8),
                preceded(pair(tag(":"), opt(space1)), map_res(take_while(|c: u8| c != b'\r'), str::from_utf8)),
            )),
            |(key, value)| Ok((key.to_string(), value.to_string())),
        ),
    )(input)?;

    let (input, _) = tag("\r\n\r\n")(input)?;

    let (input, body) = opt(take_while(|_| true))(input)?;

    Ok((
        input,
        HttpRequest {
            method: method.to_string(),
            uri: uri.to_string(),
            version: format!("HTTP/{}", version),
            headers: headers.into_iter().collect(),
            body: body.map(|b| b.to_vec()),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Usage: program <file_path>",
        ));
    }

    let mut file = File::open(&args[1])?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;

    match parse_http_request(&contents) {
        Ok((_, req)) => println!("{:#?}", req),
        Err(e) => println!("Failed to parse HTTP request: {:?}", e),
    }

    Ok(())
}