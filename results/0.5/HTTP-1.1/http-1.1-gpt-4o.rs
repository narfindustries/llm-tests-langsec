use nom::{
    bytes::complete::{tag, take_till, take_while},
    character::complete::{char, digit1, space1},
    combinator::map_res,
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

fn is_token(c: char) -> bool {
    c.is_alphanumeric() || "!#$%&'*+-.^_`|~".contains(c)
}

fn parse_token(input: &str) -> IResult<&str, &str> {
    take_while(is_token)(input)
}

fn parse_http_version(input: &str) -> IResult<&str, &str> {
    preceded(tag("HTTP/"), take_till(|c| c == '\r' || c == '\n'))(input)
}

fn parse_status_code(input: &str) -> IResult<&str, u16> {
    map_res(digit1, |s: &str| s.parse::<u16>())(input)
}

fn parse_status_line(input: &str) -> IResult<&str, (&str, u16, &str)> {
    tuple((
        parse_http_version,
        preceded(space1, parse_status_code),
        preceded(space1, take_till(|c| c == '\r' || c == '\n')),
    ))(input)
}

fn parse_header_field(input: &str) -> IResult<&str, (&str, &str)> {
    tuple((
        parse_token,
        preceded(tag(": "), take_till(|c| c == '\r' || c == '\n')),
    ))(input)
}

fn parse_headers(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
    let mut headers = Vec::new();
    let mut remaining = input;

    while let Ok((rem, header)) = parse_header_field(remaining) {
        headers.push(header);
        remaining = rem;
        let (rem, _) = char::<_, nom::error::Error<&str>>('\r')(remaining)?;
        remaining = rem;
        let (rem, _) = char::<_, nom::error::Error<&str>>('\n')(remaining)?;
        remaining = rem;
    }

    Ok((remaining, headers))
}

fn parse_http_response(input: &str) -> IResult<&str, ((&str, u16, &str), Vec<(&str, &str)>)> {
    let (input, status_line) = parse_status_line(input)?;
    let (input, _) = char::<_, nom::error::Error<&str>>('\r')(input)?;
    let (input, _) = char::<_, nom::error::Error<&str>>('\n')(input)?;
    let (input, headers) = parse_headers(input)?;
    Ok((input, (status_line, headers)))
}

fn read_file_to_string(filename: &str) -> io::Result<String> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let contents = read_file_to_string(filename)?;

    match parse_http_response(&contents) {
        Ok((_, (status_line, headers))) => {
            println!("HTTP Version: {}", status_line.0);
            println!("Status Code: {}", status_line.1);
            println!("Reason Phrase: {}", status_line.2);
            println!("Headers:");
            for (name, value) in headers {
                println!("{}: {}", name, value);
            }
        }
        Err(e) => {
            eprintln!("Failed to parse HTTP response: {:?}", e);
        }
    }

    Ok(())
}