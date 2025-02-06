use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1, take_while};
use nom::character::complete::{space0, space1};
use nom::combinator::{map, map_res};
use std::env;
use std::fs::File;
use std::io::Read;
use std::str;

#[derive(Debug, PartialEq)]
enum HttpMethod {
    GET,
    HEAD,
    POST,
    PUT,
    DELETE,
    CONNECT,
    OPTIONS,
    TRACE,
}

fn parse_http_method(input: &[u8]) -> nom::IResult<&[u8], HttpMethod> {
    alt((
        map(tag("GET"), |_| HttpMethod::GET),
        map(tag("HEAD"), |_| HttpMethod::HEAD),
        map(tag("POST"), |_| HttpMethod::POST),
        map(tag("PUT"), |_| HttpMethod::PUT),
        map(tag("DELETE"), |_| HttpMethod::DELETE),
        map(tag("CONNECT"), |_| HttpMethod::CONNECT),
        map(tag("OPTIONS"), |_| HttpMethod::OPTIONS),
        map(tag("TRACE"), |_| HttpMethod::TRACE),
    ))(input)
}

#[derive(Debug, PartialEq)]
struct HttpVersion {
    major: u8,
    minor: u8,
}

fn parse_http_version(input: &[u8]) -> nom::IResult<&[u8], HttpVersion> {
    map(
        tuple((tag("HTTP/"), take_while1(|c| c == b'0' || c == b'1'))),
        |(_, version): ((), &[u8])| HttpVersion {
            major: version[0] - b'0',
            minor: version[1] - b'0',
        },
    )(input)
}

#[derive(Debug, PartialEq)]
enum HttpStatus {
    Continue,
    SwitchingProtocols,
    Ok,
    Created,
    Accepted,
    NonAuthoritativeInformation,
    NoContent,
    ResetContent,
    PartialContent,
    MultipleChoices,
    MovedPermanently,
    Found,
    SeeOther,
    PaymentRequired,
    Forbidden,
    NotFound,
    MethodNotAllowed,
    NotAcceptable,
    ProxyAuthenticationRequired,
    RequestTimeout,
    Conflict,
    Gone,
    LengthRequired,
    PreconditionFailed,
    RequestEntityTooLarge,
    RequestURITooLarge,
    UnsupportedMediaType,
    RequestedRangeNotSatisfiable,
    ExpectationFailed,
    InternalServerError,
    NotImplemented,
    BadGateway,
    ServiceUnavailable,
    GatewayTimeout,
    HttpVersionNotSupported,
}

fn parse_http_status(input: &[u8]) -> nom::IResult<&[u8], HttpStatus> {
    alt((
        map(tag("100"), |_| HttpStatus::Continue),
        map(tag("101"), |_| HttpStatus::SwitchingProtocols),
        map(tag("200"), |_| HttpStatus::Ok),
        map(tag("201"), |_| HttpStatus::Created),
        map(tag("202"), |_| HttpStatus::Accepted),
        map(tag("203"), |_| HttpStatus::NonAuthoritativeInformation),
        map(tag("204"), |_| HttpStatus::NoContent),
        map(tag("205"), |_| HttpStatus::ResetContent),
        map(tag("206"), |_| HttpStatus::PartialContent),
        map(tag("300"), |_| HttpStatus::MultipleChoices),
        map(tag("301"), |_| HttpStatus::MovedPermanently),
        map(tag("302"), |_| HttpStatus::Found),
        map(tag("303"), |_| HttpStatus::SeeOther),
        map(tag("402"), |_| HttpStatus::PaymentRequired),
        map(tag("403"), |_| HttpStatus::Forbidden),
        map(tag("404"), |_| HttpStatus::NotFound),
        map(tag("405"), |_| HttpStatus::MethodNotAllowed),
        map(tag("406"), |_| HttpStatus::NotAcceptable),
        map(tag("407"), |_| HttpStatus::ProxyAuthenticationRequired),
        map(tag("408"), |_| HttpStatus::RequestTimeout),
        map(tag("409"), |_| HttpStatus::Conflict),
        map(tag("410"), |_| HttpStatus::Gone),
        map(tag("411"), |_| HttpStatus::LengthRequired),
        map(tag("412"), |_| HttpStatus::PreconditionFailed),
        map(tag("413"), |_| HttpStatus::RequestEntityTooLarge),
        map(tag("414"), |_| HttpStatus::RequestURITooLarge),
        map(tag("415"), |_| HttpStatus::UnsupportedMediaType),
        map(tag("416"), |_| HttpStatus::RequestedRangeNotSatisfiable),
        map(tag("417"), |_| HttpStatus::ExpectationFailed),
        map(tag("500"), |_| HttpStatus::InternalServerError),
        map(tag("501"), |_| HttpStatus::NotImplemented),
        map(tag("502"), |_| HttpStatus::BadGateway),
        map(tag("503"), |_| HttpStatus::ServiceUnavailable),
        map(tag("504"), |_| HttpStatus::GatewayTimeout),
        map(tag("505"), |_| HttpStatus::HttpVersionNotSupported),
    ))(input)
}

#[derive(Debug, PartialEq)]
struct HttpResponse {
    http_version: HttpVersion,
    status: HttpStatus,
    reason_phrase: String,
}

fn parse_http_response(input: &[u8]) -> nom::IResult<&[u8], HttpResponse> {
    map(
        tuple((
            parse_http_version,
            space1,
            parse_http_status,
            space1,
            take_while(|c| c != b'\r' && c != b'\n'),
        )),
        |(version, _, status, _, reason_phrase)| HttpResponse {
            http_version: version,
            status,
            reason_phrase: str::from_utf8(reason_phrase).unwrap().to_string(),
        },
    )(input)
}

#[derive(Debug, PartialEq)]
struct HttpRequest {
    method: HttpMethod,
    request_uri: String,
    http_version: HttpVersion,
}

fn parse_http_request(input: &[u8]) -> nom::IResult<&[u8], HttpRequest> {
    map(
        tuple((
            parse_http_method,
            space1,
            take_while(|c| c != b' '),
            space1,
            parse_http_version,
        )),
        |(method, _, request_uri, _, version)| HttpRequest {
            method,
            request_uri: str::from_utf8(request_uri).unwrap().to_string(),
            http_version: version,
        },
    )(input)
}

#[derive(Debug, PartialEq)]
enum HttpChunk {
    Chunk {
        size: u32,
        chunk: Vec<u8>,
    },
    Trailer {
        name: String,
        value: String,
    },
}

fn parse_http_chunk(input: &[u8]) -> nom::IResult<&[u8], HttpChunk> {
    alt((
        map(
            tuple((
                take_while1(|c| c.is_ascii_hexdigit()),
                space0,
                tag("\r\n"),
                take_while(|c| c != b'\r' && c != b'\n'),
                tag("\r\n"),
            )),
            |(size, _, _, chunk, _)| HttpChunk::Chunk {
                size: u32::from_str_radix(str::from_utf8(size).unwrap(), 16).unwrap(),
                chunk: chunk.to_vec(),
            },
        ),
        map(
            tuple((
                take_while(|c| c != b':'),
                tag(":"),
                space0,
                take_while(|c| c != b'\r' && c != b'\n'),
                tag("\r\n"),
            )),
            |(name, _, _, value, _)| HttpChunk::Trailer {
                name: str::from_utf8(name).unwrap().to_string(),
                value: str::from_utf8(value).unwrap().to_string(),
            },
        ),
    ))(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut input = Vec::new();
    file.read_to_end(&mut input)?;

    match parse_http_request(input.as_slice()) {
        Ok((remaining, request)) => {
            println!("Request: {:?}", request);
            match parse_http_response(remaining) {
                Ok((remaining, response)) => {
                    println!("Response: {:?}", response);
                    let mut chunks = Vec::new();
                    let mut remaining = remaining;
                    while !remaining.is_empty() {
                        match parse_http_chunk(remaining) {
                            Ok((remaining_input, chunk)) => {
                                chunks.push(chunk);
                                remaining = remaining_input;
                            }
                            Err(_) => break,
                        }
                    }
                    println!("Chunks: {:?}", chunks);
                }
                Err(err) => println!("Error parsing response: {:?}", err),
            }
        }
        Err(err) => println!("Error parsing request: {:?}", err),
    }

    Ok(())
}