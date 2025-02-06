use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, digit1, multispace0, multispace1},
    combinator::{map, map_res, value},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::str;

#[derive(Debug, PartialEq, Clone)]
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

fn http_method(input: &[u8]) -> IResult<&[u8], HttpMethod> {
    alt((
        value(HttpMethod::GET, tag("GET")),
        value(HttpMethod::HEAD, tag("HEAD")),
        value(HttpMethod::POST, tag("POST")),
        value(HttpMethod::PUT, tag("PUT")),
        value(HttpMethod::DELETE, tag("DELETE")),
        value(HttpMethod::CONNECT, tag("CONNECT")),
        value(HttpMethod::OPTIONS, tag("OPTIONS")),
        value(HttpMethod::TRACE, tag("TRACE")),
    ))(input)
}

#[derive(Debug, PartialEq)]
enum HttpVersion {
    HTTP11,
}

fn http_version(input: &[u8]) -> IResult<&[u8], HttpVersion> {
    map_res(
        tag("HTTP/1.1"),
        |_| Ok(HttpVersion::HTTP11),
    )(input)
}

#[derive(Debug, PartialEq)]
enum HttpStatus {
    Informational(u16),
    Successful(u16),
    Redirection(u16),
    ClientError(u16),
    ServerError(u16),
}

fn http_status(input: &[u8]) -> IResult<&[u8], HttpStatus> {
    let (input, code) = map_res(digit1, |s: &[u8]| str::from_utf8(s).unwrap().parse::<u16>())(input)?;
    match code {
        100..=199 => Ok((input, HttpStatus::Informational(code))),
        200..=299 => Ok((input, HttpStatus::Successful(code))),
        300..=399 => Ok((input, HttpStatus::Redirection(code))),
        400..=499 => Ok((input, HttpStatus::ClientError(code))),
        500..=599 => Ok((input, HttpStatus::ServerError(code))),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::Digit)),
    }
}

#[derive(Debug, PartialEq)]
enum HttpHeader {
    Accept(String),
    AcceptCharset(String),
    AcceptEncoding(String),
    AcceptLanguage(String),
    Authorization(String),
    CacheControl(String),
    Connection(String),
    ContentLength(u64),
    ContentType(String),
    Date(String),
    Expect(String),
    From(String),
    Host(String),
    IfMatch(String),
    IfModifiedSince(String),
    IfNoneMatch(String),
    IfRange(String),
    IfUnmodifiedSince(String),
    MaxForwards(u16),
    ProxyAuthorization(String),
    Range(String),
    Referer(String),
    TE(String),
    UserAgent(String),
    Via(String),
    Warning(String),
    WWWAuthenticate(String),
}

fn http_header(input: &[u8]) -> IResult<&[u8], HttpHeader> {
    alt((
        map(preceded(tag("Accept: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::Accept(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Accept-Charset: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::AcceptCharset(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Accept-Encoding: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::AcceptEncoding(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Accept-Language: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::AcceptLanguage(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Authorization: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::Authorization(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Cache-Control: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::CacheControl(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Connection: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::Connection(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Content-Length: "), map_res(digit1, |s: &[u8]| str::from_utf8(s).unwrap().parse::<u64>())), |n| HttpHeader::ContentLength(n)),
        map(preceded(tag("Content-Type: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::ContentType(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Date: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::Date(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Expect: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::Expect(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("From: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::From(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Host: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::Host(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("If-Match: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::IfMatch(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("If-Modified-Since: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::IfModifiedSince(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("If-None-Match: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::IfNoneMatch(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("If-Range: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::IfRange(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("If-Unmodified-Since: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::IfUnmodifiedSince(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Max-Forwards: "), map_res(digit1, |s: &[u8]| str::from_utf8(s).unwrap().parse::<u16>())), |n| HttpHeader::MaxForwards(n)),
        map(preceded(tag("Proxy-Authorization: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::ProxyAuthorization(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Range: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::Range(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Referer: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::Referer(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("TE: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::TE(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("User-Agent: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::UserAgent(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Via: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::Via(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("Warning: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::Warning(String::from_utf8_lossy(s).into_owned())),
        map(preceded(tag("WWW-Authenticate: "), take_while1(|c| c != b'\r' && c != b'\n')), |s| HttpHeader::WWWAuthenticate(String::from_utf8_lossy(s).into_owned())),
    ))(input)
}

#[derive(Debug, PartialEq)]
enum HttpRequest {
    RequestLine(HttpMethod, String, HttpVersion),
    Headers(Vec<HttpHeader>),
}

fn http_request(input: &[u8]) -> IResult<&[u8], HttpRequest> {
    alt((
        map(
            tuple((
                http_method,
                multispace1,
                take_while1(|c| c != b' ' && c != b'\r' && c != b'\n'),
                multispace1,
                http_version,
                tag("\r\n"),
            )),
            |(method, _, path, _, version, _)| HttpRequest::RequestLine(method, String::from_utf8_lossy(path).into_owned(), version),
        ),
        map(
            many0(alt((http_header, map(tag("\r\n"), |_| ())))),
            |headers| HttpRequest::Headers(headers.into_iter().filter_map(|h| h).collect()),
        ),
    ))(input)
}

#[derive(Debug, PartialEq)]
enum HttpResponse {
    StatusLine(HttpStatus, String),
    Headers(Vec<HttpHeader>),
}

fn http_response(input: &[u8]) -> IResult<&[u8], HttpResponse> {
    alt((
        map(
            tuple((
                http_version,
                multispace1,
                http_status,
                multispace1,
                take_while1(|c| c != b'\r' && c != b'\n'),
                tag("\r\n"),
            )),
            |(version, _, status, _, reason, _)| HttpResponse::StatusLine(status, String::from_utf8_lossy(reason).into_owned()),
        ),
        map(
            many0(alt((http_header, map(tag("\r\n"), |_| ())))),
            |headers| HttpResponse::Headers(headers.into_iter().filter_map(|h| h).collect()),
        ),
    ))(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let mut file = File::open(file_path).unwrap();
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).unwrap();
    match http_request(&buffer) {
        Ok((_remaining, request)) => println!("{:?}", request),
        Err(e) => println!("{:?}", e),
    }
    match http_response(&buffer) {
        Ok((_remaining, response)) => println!("{:?}", response),
        Err(e) => println!("{:?}", e),
    }
}