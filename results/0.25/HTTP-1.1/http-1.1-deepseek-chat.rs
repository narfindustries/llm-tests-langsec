use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, crlf, digit1, line_ending, space0, space1},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

type Header = (String, String);

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    let (input, method) = map_res(take_while1(|c| c != b' '), |s: &[u8]| {
        std::str::from_utf8(s).map(|s| s.to_string())
    })(input)?;
    let (input, _) = space1(input)?;
    let (input, uri) = map_res(take_while1(|c| c != b' '), |s: &[u8]| {
        std::str::from_utf8(s).map(|s| s.to_string())
    })(input)?;
    let (input, _) = space1(input)?;
    let (input, version) = map_res(take_while1(|c| c != b'\r'), |s: &[u8]| {
        std::str::from_utf8(s).map(|s| s.to_string())
    })(input)?;
    let (input, _) = crlf(input)?;
    Ok((input, (method, uri, version)))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    let (input, key) = map_res(take_while1(|c| c != b':'), |s: &[u8]| {
        std::str::from_utf8(s).map(|s| s.to_string())
    })(input)?;
    let (input, _) = pair(char(':'), space0)(input)?;
    let (input, value) = map_res(take_while1(|c| c != b'\r'), |s: &[u8]| {
        std::str::from_utf8(s).map(|s| s.to_string())
    })(input)?;
    let (input, _) = crlf(input)?;
    Ok((input, (key, value)))
}

fn parse_headers(input: &[u8]) -> IResult<&[u8], Vec<Header>> {
    many0(parse_header)(input)
}

fn parse_request(input: &[u8]) -> IResult<&[u8], (String, String, String, Vec<Header>)> {
    let (input, (method, uri, version)) = parse_request_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, _) = crlf(input)?;
    Ok((input, (method, uri, version, headers)))
}

fn parse_status_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    let (input, version) = map_res(take_while1(|c| c != b' '), |s: &[u8]| {
        std::str::from_utf8(s).map(|s| s.to_string())
    })(input)?;
    let (input, _) = space1(input)?;
    let (input, status_code) = map_res(digit1, |s: &[u8]| {
        std::str::from_utf8(s).map(|s| s.to_string())
    })(input)?;
    let (input, _) = space1(input)?;
    let (input, reason_phrase) = map_res(take_while1(|c| c != b'\r'), |s: &[u8]| {
        std::str::from_utf8(s).map(|s| s.to_string())
    })(input)?;
    let (input, _) = crlf(input)?;
    Ok((input, (version, status_code, reason_phrase)))
}

fn parse_response(input: &[u8]) -> IResult<&[u8], (String, String, String, Vec<Header>)> {
    let (input, (version, status_code, reason_phrase)) = parse_status_line(input)?;
    let (input, headers) = parse_headers(input)?;
    let (input, _) = crlf(input)?;
    Ok((input, (version, status_code, reason_phrase, headers)))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let result = alt((parse_request, parse_response))(&buffer);
    match result {
        Ok((_, (method_or_version, uri_or_status, version_or_reason, headers))) => {
            println!("Method/Version: {}", method_or_version);
            println!("URI/Status: {}", uri_or_status);
            println!("Version/Reason: {}", version_or_reason);
            for (key, value) in headers {
                println!("{}: {}", key, value);
            }
        }
        Err(e) => eprintln!("Failed to parse: {:?}", e),
    }

    Ok(())
}