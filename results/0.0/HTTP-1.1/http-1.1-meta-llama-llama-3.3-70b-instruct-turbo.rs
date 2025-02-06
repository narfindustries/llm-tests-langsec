use nom::{
    branch::alt,
    bytes::complete::{tag, take_while_m_n},
    character::complete::{char, digit1, multispace0, multispace1},
    combinator::{map, map_res, opt, recognize},
    error::{Error, ErrorKind},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, preceded, separated_pair, tuple},
    IResult,
};
use std::{
    collections::HashMap,
    env,
    fmt,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

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

impl HttpMethod {
    fn parse(input: &str) -> IResult<&str, HttpMethod> {
        alt((
            tag("GET"),
            tag("HEAD"),
            tag("POST"),
            tag("PUT"),
            tag("DELETE"),
            tag("CONNECT"),
            tag("OPTIONS"),
            tag("TRACE"),
        ))
        .map(|method| match method {
            "GET" => HttpMethod::GET,
            "HEAD" => HttpMethod::HEAD,
            "POST" => HttpMethod::POST,
            "PUT" => HttpMethod::PUT,
            "DELETE" => HttpMethod::DELETE,
            "CONNECT" => HttpMethod::CONNECT,
            "OPTIONS" => HttpMethod::OPTIONS,
            "TRACE" => HttpMethod::TRACE,
            _ => unreachable!(),
        })(input)
    }
}

impl fmt::Display for HttpMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HttpMethod::GET => write!(f, "GET"),
            HttpMethod::HEAD => write!(f, "HEAD"),
            HttpMethod::POST => write!(f, "POST"),
            HttpMethod::PUT => write!(f, "PUT"),
            HttpMethod::DELETE => write!(f, "DELETE"),
            HttpMethod::CONNECT => write!(f, "CONNECT"),
            HttpMethod::OPTIONS => write!(f, "OPTIONS"),
            HttpMethod::TRACE => write!(f, "TRACE"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum HttpVersion {
    V1_1,
}

impl HttpVersion {
    fn parse(input: &str) -> IResult<&str, HttpVersion> {
        tag("HTTP/1.1").map(|_| HttpVersion::V1_1)(input)
    }
}

impl fmt::Display for HttpVersion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HttpVersion::V1_1 => write!(f, "HTTP/1.1"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum RequestUri {
    Absolute(String),
    Relative(String),
    Asterisk,
}

impl RequestUri {
    fn parse(input: &str) -> IResult<&str, RequestUri> {
        alt((
            recognize(preceded(
                char('/'),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == '/' || c == '?' || c == '#'),
            ))
            .map(|uri| RequestUri::Relative(uri.to_string())),
            recognize(preceded(
                tag("http://"),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == '/' || c == '?' || c == '#'),
            ))
            .map(|uri| RequestUri::Absolute(uri.to_string())),
            tag("*").map(|_| RequestUri::Asterisk),
        ))(input)
    }
}

impl fmt::Display for RequestUri {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RequestUri::Absolute(uri) => write!(f, "{}", uri),
            RequestUri::Relative(uri) => write!(f, "{}", uri),
            RequestUri::Asterisk => write!(f, "*"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Header {
    Accept(String),
    AcceptCharset(String),
    AcceptEncoding(String),
    AcceptLanguage(String),
    Authorization(String),
    Expect(String),
    From(String),
    Host(String),
    IfMatch(String),
    IfModifiedSince(String),
    IfNoneMatch(String),
    IfRange(String),
    IfUnmodifiedSince(String),
    MaxForwards(u32),
    ProxyAuthorization(String),
    Range(String),
    Referer(String),
    Te(String),
    UserAgent(String),
}

impl Header {
    fn parse(input: &str) -> IResult<&str, Header> {
        alt((
            preceded(
                tag("Accept: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == '/' || c == ';' || c == ','),
            )
            .map(|value| Header::Accept(value.to_string())),
            preceded(
                tag("Accept-Charset: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ';' || c == ','),
            )
            .map(|value| Header::AcceptCharset(value.to_string())),
            preceded(
                tag("Accept-Encoding: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ';' || c == ','),
            )
            .map(|value| Header::AcceptEncoding(value.to_string())),
            preceded(
                tag("Accept-Language: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ';' || c == ','),
            )
            .map(|value| Header::AcceptLanguage(value.to_string())),
            preceded(
                tag("Authorization: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ' ' || c == ';'),
            )
            .map(|value| Header::Authorization(value.to_string())),
            preceded(
                tag("Expect: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ';'),
            )
            .map(|value| Header::Expect(value.to_string())),
            preceded(
                tag("From: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == '@' || c == ';'),
            )
            .map(|value| Header::From(value.to_string())),
            preceded(
                tag("Host: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ':' || c == ';'),
            )
            .map(|value| Header::Host(value.to_string())),
            preceded(
                tag("If-Match: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ';' || c == ','),
            )
            .map(|value| Header::IfMatch(value.to_string())),
            preceded(
                tag("If-Modified-Since: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ':' || c == ';'),
            )
            .map(|value| Header::IfModifiedSince(value.to_string())),
            preceded(
                tag("If-None-Match: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ';' || c == ','),
            )
            .map(|value| Header::IfNoneMatch(value.to_string())),
            preceded(
                tag("If-Range: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ';' || c == ','),
            )
            .map(|value| Header::IfRange(value.to_string())),
            preceded(
                tag("If-Unmodified-Since: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ':' || c == ';'),
            )
            .map(|value| Header::IfUnmodifiedSince(value.to_string())),
            preceded(
                tag("Max-Forwards: "),
                map_res(digit1, |value: &str| value.parse::<u32>()),
            )
            .map(|value| Header::MaxForwards(value)),
            preceded(
                tag("Proxy-Authorization: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ' ' || c == ';'),
            )
            .map(|value| Header::ProxyAuthorization(value.to_string())),
            preceded(
                tag("Range: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == '-' || c == ';'),
            )
            .map(|value| Header::Range(value.to_string())),
            preceded(
                tag("Referer: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ':' || c == ';'),
            )
            .map(|value| Header::Referer(value.to_string())),
            preceded(
                tag("TE: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ';' || c == ','),
            )
            .map(|value| Header::Te(value.to_string())),
            preceded(
                tag("User-Agent: "),
                take_while_m_n(1, 1024, |c: char| c.is_ascii_alphanumeric() || c == ' ' || c == ';'),
            )
            .map(|value| Header::UserAgent(value.to_string())),
        ))(input)
    }
}

impl fmt::Display for Header {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Header::Accept(value) => write!(f, "Accept: {}", value),
            Header::AcceptCharset(value) => write!(f, "Accept-Charset: {}", value),
            Header::AcceptEncoding(value) => write!(f, "Accept-Encoding: {}", value),
            Header::AcceptLanguage(value) => write!(f, "Accept-Language: {}", value),
            Header::Authorization(value) => write!(f, "Authorization: {}", value),
            Header::Expect(value) => write!(f, "Expect: {}", value),
            Header::From(value) => write!(f, "From: {}", value),
            Header::Host(value) => write!(f, "Host: {}", value),
            Header::IfMatch(value) => write!(f, "If-Match: {}", value),
            Header::IfModifiedSince(value) => write!(f, "If-Modified-Since: {}", value),
            Header::IfNoneMatch(value) => write!(f, "If-None-Match: {}", value),
            Header::IfRange(value) => write!(f, "If-Range: {}", value),
            Header::IfUnmodifiedSince(value) => write!(f, "If-Unmodified-Since: {}", value),
            Header::MaxForwards(value) => write!(f, "Max-Forwards: {}", value),
            Header::ProxyAuthorization(value) => write!(f, "Proxy-Authorization: {}", value),
            Header::Range(value) => write!(f, "Range: {}", value),
            Header::Referer(value) => write!(f, "Referer: {}", value),
            Header::Te(value) => write!(f, "TE: {}", value),
            Header::UserAgent(value) => write!(f, "User-Agent: {}", value),
        }
    }
}

#[derive(Debug, PartialEq)]
enum StatusCode {
    Informational(u16),
    Successful(u16),
    Redirection(u16),
    ClientError(u16),
    ServerError(u16),
}

impl StatusCode {
    fn parse(input: &str) -> IResult<&str, StatusCode> {
        let (input, code) = map_res(digit1, |value: &str| value.parse::<u16>())(input)?;
        match code {
            100 => Ok((input, StatusCode::Informational(code))),
            101 => Ok((input, StatusCode::Informational(code))),
            200 => Ok((input, StatusCode::Successful(code))),
            201 => Ok((input, StatusCode::Successful(code))),
            202 => Ok((input, StatusCode::Successful(code))),
            203 => Ok((input, StatusCode::Successful(code))),
            204 => Ok((input, StatusCode::Successful(code))),
            205 => Ok((input, StatusCode::Successful(code))),
            206 => Ok((input, StatusCode::Successful(code))),
            300 => Ok((input, StatusCode::Redirection(code))),
            301 => Ok((input, StatusCode::Redirection(code))),
            302 => Ok((input, StatusCode::Redirection(code))),
            303 => Ok((input, StatusCode::Redirection(code))),
            304 => Ok((input, StatusCode::Redirection(code))),
            305 => Ok((input, StatusCode::Redirection(code))),
            307 => Ok((input, StatusCode::Redirection(code))),
            400 => Ok((input, StatusCode::ClientError(code))),
            401 => Ok((input, StatusCode::ClientError(code))),
            402 => Ok((input, StatusCode::ClientError(code))),
            403 => Ok((input, StatusCode::ClientError(code))),
            404 => Ok((input, StatusCode::ClientError(code))),
            405 => Ok((input, StatusCode::ClientError(code))),
            406 => Ok((input, StatusCode::ClientError(code))),
            407 => Ok((input, StatusCode::ClientError(code))),
            408 => Ok((input, StatusCode::ClientError(code))),
            409 => Ok((input, StatusCode::ClientError(code))),
            410 => Ok((input, StatusCode::ClientError(code))),
            411 => Ok((input, StatusCode::ClientError(code))),
            412 => Ok((input, StatusCode::ClientError(code))),
            413 => Ok((input, StatusCode::ClientError(code))),
            414 => Ok((input, StatusCode::ClientError(code))),
            415 => Ok((input, StatusCode::ClientError(code))),
            416 => Ok((input, StatusCode::ClientError(code))),
            417 => Ok((input, StatusCode::ClientError(code))),
            500 => Ok((input, StatusCode::ServerError(code))),
            501 => Ok((input, StatusCode::ServerError(code))),
            502 => Ok((input, StatusCode::ServerError(code))),
            503 => Ok((input, StatusCode::ServerError(code))),
            504 => Ok((input, StatusCode::ServerError(code))),
            505 => Ok((input, StatusCode::ServerError(code))),
            _ => Err(nom::Err::Error((input, ErrorKind::Digit))),
        }
    }
}

impl fmt::Display for StatusCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StatusCode::Informational(code) => write!(f, "{}", code),
            StatusCode::Successful(code) => write!(f, "{}", code),
            StatusCode::Redirection(code) => write!(f, "{}", code),
            StatusCode::ClientError(code) => write!(f, "{}", code),
            StatusCode::ServerError(code) => write!(f, "{}", code),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ReasonPhrase {
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
    NotModified,
    UseProxy,
    TemporaryRedirect,
    BadRequest,
    Unauthorized,
    PaymentRequired,
    Forbidden,
    NotFound,
    MethodNotAllowed,
    NotAcceptable,
    ProxyAuthenticationRequired,
    RequestTimeOut,
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
    GatewayTimeOut,
    HTTPVersionNotSupported,
}

impl ReasonPhrase {
    fn parse(input: &str) -> IResult<&str, ReasonPhrase> {
        alt((
            tag("OK"),
            tag("Created"),
            tag("Accepted"),
            tag("Non-Authoritative Information"),
            tag("No Content"),
            tag("Reset Content"),
            tag("Partial Content"),
            tag("Multiple Choices"),
            tag("Moved Permanently"),
            tag("Found"),
            tag("See Other"),
            tag("Not Modified"),
            tag("Use Proxy"),
            tag("Temporary Redirect"),
            tag("Bad Request"),
            tag("Unauthorized"),
            tag("Payment Required"),
            tag("Forbidden"),
            tag("Not Found"),
            tag("Method Not Allowed"),
            tag("Not Acceptable"),
            tag("Proxy Authentication Required"),
            tag("Request Time-out"),
            tag("Conflict"),
            tag("Gone"),
            tag("Length Required"),
            tag("Precondition Failed"),
            tag("Request Entity Too Large"),
            tag("Request-URI Too Large"),
            tag("Unsupported Media Type"),
            tag("Requested Range Not Satisfiable"),
            tag("Expectation Failed"),
            tag("Internal Server Error"),
            tag("Not Implemented"),
            tag("Bad Gateway"),
            tag("Service Unavailable"),
            tag("Gateway Time-out"),
            tag("HTTP Version not supported"),
        ))
        .map(|phrase| match phrase {
            "OK" => ReasonPhrase::Ok,
            "Created" => ReasonPhrase::Created,
            "Accepted" => ReasonPhrase::Accepted,
            "Non-Authoritative Information" => ReasonPhrase::NonAuthoritativeInformation,
            "No Content" => ReasonPhrase::NoContent,
            "Reset Content" => ReasonPhrase::ResetContent,
            "Partial Content" => ReasonPhrase::PartialContent,
            "Multiple Choices" => ReasonPhrase::MultipleChoices,
            "Moved Permanently" => ReasonPhrase::MovedPermanently,
            "Found" => ReasonPhrase::Found,
            "See Other" => ReasonPhrase::SeeOther,
            "Not Modified" => ReasonPhrase::NotModified,
            "Use Proxy" => ReasonPhrase::UseProxy,
            "Temporary Redirect" => ReasonPhrase::TemporaryRedirect,
            "Bad Request" => ReasonPhrase::BadRequest,
            "Unauthorized" => ReasonPhrase::Unauthorized,
            "Payment Required" => ReasonPhrase::PaymentRequired,
            "Forbidden" => ReasonPhrase::Forbidden,
            "Not Found" => ReasonPhrase::NotFound,
            "Method Not Allowed" => ReasonPhrase::MethodNotAllowed,
            "Not Acceptable" => ReasonPhrase::NotAcceptable,
            "Proxy Authentication Required" => ReasonPhrase::ProxyAuthenticationRequired,
            "Request Time-out" => ReasonPhrase::RequestTimeOut,
            "Conflict" => ReasonPhrase::Conflict,
            "Gone" => ReasonPhrase::Gone,
            "Length Required" => ReasonPhrase::LengthRequired,
            "Precondition Failed" => ReasonPhrase::PreconditionFailed,
            "Request Entity Too Large" => ReasonPhrase::RequestEntityTooLarge,
            "Request-URI Too Large" => ReasonPhrase::RequestURITooLarge,
            "Unsupported Media Type" => ReasonPhrase::UnsupportedMediaType,
            "