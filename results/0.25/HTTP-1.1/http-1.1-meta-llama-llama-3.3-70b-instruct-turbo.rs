use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, space0, space1},
    combinator::{map, opt},
    multi::{many0, many1},
    sequence::{delimited, preceded, separated_pair, tuple},
    IResult,
};
use nom::error::ErrorKind;

fn crlf(input: &[u8]) -> IResult<&[u8], &[u8]> {
    tag("\r\n")(input)
}

fn parse_token(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    take_while1(|c| match c {
        b' ' | b'\t' | b'\r' | b'\n' | b'(' | b')' | b'<' | b'>' | b'@' | b',' | b';' | b':' | b'\\' | b'\"' | b'/' | b'[' | b']' | b'?' | b'=' | b'{' | b'}' | b' ' => false,
        _ => true,
    })(input)
}

fn parse_quoted_string(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    preceded(
        char('\"'),
        delimited(
            space0,
            take_while1(|c| *c != b'\"' && *c != b'\\'),
            delimited(space0, tag("\\"), space0),
        ),
    )(input)
    .or_delimited(
        char('\"'),
        alt((parse_token, parse_quoted_string)),
        char('\"'),
    )
}

fn parse_field_value(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    alt((parse_token, parse_quoted_string))(input)
}

fn parse_field(input: &[u8]) -> IResult<&[u8], (String, String)> {
    separated_pair(
        map(parse_token, |v| String::from_utf8_lossy(v).into_owned()),
        tag(":"),
        parse_field_value,
    )(input)
}

fn parse_fields(input: &[u8]) -> IResult<&[u8], Vec<(String, String)>> {
    many0(preceded(space0, parse_field))(input)
}

fn parse_request_line(input: &[u8]) -> IResult<&[u8], (String, String, String)> {
    tuple((
        map(parse_token, |v| String::from_utf8_lossy(v).into_owned()),
        map(parse_token, |v| String::from_utf8_lossy(v).into_owned()),
        map(
            delimited(tag("HTTP/"), parse_token, tag("\r\n")),
            |v| String::from_utf8_lossy(v).into_owned(),
        ),
    ))(input)
}

fn parse_response_line(input: &[u8]) -> IResult<&[u8], (String, u16, String)> {
    tuple((
        map(tag("HTTP/"), |_| String::from("HTTP")),
        map(delimited(tag(" "), parse_token, tag(" ")), |v| {
            String::from_utf8_lossy(v)
                .parse::<u16>()
                .unwrap()
        }),
        map(delimited(space1, parse_token, crlf), |v| {
            String::from_utf8_lossy(v).into_owned()
        }),
    ))(input)
}

fn parse_request(input: &[u8]) -> IResult<&[u8], (String, String, String, Vec<(String, String)>)> {
    tuple((
        parse_request_line,
        parse_fields,
        opt(preceded(crlf, parse_fields)),
        crlf,
    ))(input)
}

fn parse_response(input: &[u8]) -> IResult<&[u8], (String, u16, String, Vec<(String, String)>)> {
    tuple((
        parse_response_line,
        parse_fields,
        opt(preceded(crlf, parse_fields)),
        crlf,
    ))(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <file>", args[0]);
    }
    let mut file = File::open(&args[1])?;
    let mut input = Vec::new();
    file.read_to_end(&mut input)?;
    let result = alt((parse_request, parse_response))(&input);
    match result {
        Ok((remaining, value)) => {
            println!("{:?}", value);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
    Ok(())
}