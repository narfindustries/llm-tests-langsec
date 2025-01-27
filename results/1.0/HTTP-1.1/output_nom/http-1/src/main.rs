use nom::{
    bytes::complete::{take_till, take_while_m_n},
    character::complete::{char, digit1, multispace0, multispace1},
    combinator::{map, map_res, opt},
    error::{ErrorKind, VerboseError},
    multi::{many0, separated_list0},
    sequence::{delimited, separated_pair, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
    str,
};

// Parse HTTP version
fn http_version(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8], &[u8])> {
    tuple((char('H'), char('T'), char('T'), char('P'), char('/'), take_while_m_n(1, 1, |c| c == b'1' || c == b'0'), char('/'), digit1))(input)
}

// Parse HTTP method
fn http_method(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while_m_n(1, 10, |c| (c >= b'A' && c <= b'Z') || (c >= b'a' && c <= b'z') || c == b' ' || c == b'!')(input)
}

// Parse HTTP request-target
fn http_request_target(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_till(|c| c == b' ' || c == b'\t')(input)
}

// Parse HTTP request
fn http_request(input: &[u8]) -> IResult<&[u8], (&[u8], (&[u8], &[u8], &[u8]), &[u8])> {
    tuple((http_method, http_request_target, http_version))(input)
}

// Parse HTTP field-name
fn http_field_name(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while_m_n(1, 50, |c| (c >= b'A' && c <= b'Z') || (c >= b'a' && c <= b'z') || (c >= b'0' && c <= b'9') || c == b'-' || c == b'_')(input)
}

// Parse HTTP field-value
fn http_field_value(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while_m_n(1, 1000, |c| c != b'\r' && c != b'\n')(input)
}

// Parse HTTP header field
fn http_header_field(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8])> {
    separated_pair(http_field_name, char(':'), http_field_value)(input)
}

// Parse HTTP headers
fn http_headers(input: &[u8]) -> IResult<&[u8], Vec<(&[u8], &[u8])>> {
    many0(delimited(multispace0, http_header_field, multispace0))(input)
}

// Parse HTTP message body
fn http_message_body(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while_m_n(0, input.len(), |c| true)(input)
}

// Parse HTTP response-status-code
fn http_response_status_code(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while_m_n(1, 3, |c| c >= b'0' && c <= b'9')(input)
}

// Parse HTTP reason-phrase
fn http_reason_phrase(input: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while_m_n(1, 100, |c| c != b'\r' && c != b'\n')(input)
}

// Parse HTTP response
fn http_response(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8], &[u8], Vec<(&[u8], &[u8])>, &[u8])> {
    tuple((http_version, http_response_status_code, http_reason_phrase, http_headers, http_message_body))(input)
}

// Parse HTTP message
fn http_message(input: &[u8]) -> IResult<&[u8], (&[u8], (&[u8], &[u8], &[u8]), &[u8], Vec<(&[u8], &[u8])>, &[u8])> {
    tuple((http_request, multispace1, http_headers, http_message_body))(input)
}

// Parse HTTP response message
fn http_response_message(input: &[u8]) -> IResult<&[u8], (&[u8], &[u8], &[u8], Vec<(&[u8], &[u8])>, &[u8])> {
    tuple((http_version, http_response_status_code, http_reason_phrase, http_headers, http_message_body))(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Please provide the input file as a command line argument.");
    }
    let file_name = &args[1];
    let file = File::open(file_name).expect("Failed to open file.");
    let mut reader = BufReader::new(file);
    let mut data = Vec::new();
    reader.read_to_end(&mut data).expect("Failed to read file.");
    match http_message(&data) {
        Ok((_, msg)) => println!("{:?}", msg),
        Err(err) => panic!("{:?}", err),
    }
}