domain http_1_1 {
  syntax request = 
    method: string @("GET" | "HEAD" | "POST" | "PUT" | "DELETE" | "CONNECT" | "OPTIONS" | "TRACE"),
    sp: string @(" "),
    request_uri: string,
    sp: string @(" "),
    http_version: string @("HTTP/1.1"),
    crlf: string @("\r\n"),
    headers: header*,
    crlf: string @("\r\n"),
    body: bytes;

  syntax response = 
    http_version: string @("HTTP/1.1"),
    sp: string @(" "),
    status_code: uint @(
      100 | 101 | 200 | 201 | 202 | 203 | 204 | 205 | 206 | 
      300 | 301 | 302 | 303 | 304 | 305 | 307 | 
      400 | 401 | 402 | 403 | 404 | 405 | 406 | 407 | 408 | 409 | 410 | 411 | 412 | 413 | 414 | 415 | 416 | 417 | 
      500 | 501 | 502 | 503 | 504 | 505
    ),
    sp: string @(" "),
    reason_phrase: string,
    crlf: string @("\r\n"),
    headers: header*,
    crlf: string @("\r\n"),
    body: bytes;

  syntax header = 
    field_name: string @(
      "Accept" | "Accept-Charset" | "Accept-Encoding" | "Accept-Language" | 
      "Authorization" | "Cache-Control" | "Connection" | "Content-Encoding" | 
      "Content-Language" | "Content-Length" | "Content-Type" | "Date" | 
      "Expect" | "From" | "Host" | "If-Match" | "If-Modified-Since" | 
      "If-None-Match" | "If-Range" | "If-Unmodified-Since" | "Max-Forwards" | 
      "Proxy-Authorization" | "Range" | "Referer" | "TE" | "Upgrade" | 
      "User-Agent" | "Via" | "Warning"
    ),
    colon: string @(":"),
    sp: string @(" "),
    field_value: string,
    crlf: string @("\r\n");

  syntax entity_header = 
    allow: string,
    content_base: string,
    content_encoding: string,
    content_language: string,
    content_length: uint,
    content_location: string,
    content_md5: string,
    content_range: string,
    content_type: string,
    expires: string,
    last_modified: string;

  syntax entity_body = bytes;
}