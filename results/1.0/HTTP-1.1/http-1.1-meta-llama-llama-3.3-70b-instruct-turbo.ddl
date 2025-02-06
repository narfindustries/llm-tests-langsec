format binary

http_message = 
  choice 
    request: 
      sequence 
        method: http_method,
        request_uri: string,
        http_version: string,
        headers: http_headers,
        crlf: 0x0D 0x0A,
        body: bytes 
          if request_header["Content-Length"] != 0,
    response: 
      sequence 
        http_version: string,
        status_code: status_code,
        reason_phrase: string,
        crlf: 0x0D 0x0A,
        headers: http_headers,
        crlf: 0x0D 0x0A,
        body: bytes 
          if response_header["Content-Length"] != 0

http_method = 
  choice 
    GET: string "GET",
    HEAD: string "HEAD",
    POST: string "POST",
    PUT: string "PUT",
    DELETE: string "DELETE",
    CONNECT: string "CONNECT",
    OPTIONS: string "OPTIONS",
    TRACE: string "TRACE"

request_header = 
  sequence 
    accept: string,
    accept_charset: string,
    accept_encoding: string,
    accept_language: string,
    authorization: string,
    host: string,
    if_match: string,
    if_modified_since: string,
    if_none_match: string,
    if_range: string,
    if_unmodified_since: string,
    max_forwards: uint16,
    proxy_authorization: string,
    range: string,
    referer: string,
    te: string,
    user_agent: string,
    content_length: uint32,
    content_type: string

response_header = 
  sequence 
    age: uint32,
    etag: string,
    last_modified: string,
    location: string,
    proxy_authentication: string,
    retry_after: uint32,
    server: string,
    vary: string,
    www_authenticate: string,
    warning: string,
    content_length: uint32,
    content_type: string,
    content_encoding: string,
    content_language: string,
    content_location: string,
    content_md5: string,
    content_range: string,
    expires: string,
    cache_control: string

http_headers = 
  repeat 
    header_name: string,
    colon: 0x3A,
    space: 0x20,
    header_value: string,
    crlf: 0x0D 0x0A

status_code = 
  choice 
    CONTINUE: uint16 100,
    SWITCHING_PROTOCOLS: uint16 101,
    OK: uint16 200,
    CREATED: uint16 201,
    ACCEPTED: uint16 202,
    NON_AUTHORITATIVE_INFORMATION: uint16 203,
    NO_CONTENT: uint16 204,
    RESET_CONTENT: uint16 205,
    PARTIAL_CONTENT: uint16 206,
    MULTIPLE_CHOICES: uint16 300,
    MOVED_PERMANENTLY: uint16 301,
    FOUND: uint16 302,
    SEE_OTHER: uint16 303,
    NOT_MODIFIED: uint16 304,
    USE_PROXY: uint16 305,
    TEMPORARY_REDIRECT: uint16 307,
    BAD_REQUEST: uint16 400,
    UNAUTHORIZED: uint16 401,
    PAYMENT_REQUIRED: uint16 402,
    FORBIDDEN: uint16 403,
    NOT_FOUND: uint16 404,
    METHOD_NOT_ALLOWED: uint16 405,
    NOT_ACCEPTABLE: uint16 406,
    PROXY_AUTHENTICATION_REQUIRED: uint16 407,
    REQUEST_TIME_OUT: uint16 408,
    CONFLICT: uint16 409,
    GONE: uint16 410,
    LENGTH_REQUIRED: uint16 411,
    PRECONDITION_FAILED: uint16 412,
    REQUEST_ENTITY_TOO_LARGE: uint16 413,
    REQUEST_URI_TOO_LARGE: uint16 414,
    UNSUPPORTED_MEDIA_TYPE: uint16 415,
    REQUESTED_RANGE_NOT_SATISFIABLE: uint16 416,
    EXPECTATION_FAILED: uint16 417,
    INTERNAL_SERVER_ERROR: uint16 500,
    NOT_IMPLEMENTED: uint16 501,
    BAD_GATEWAY: uint16 502,
    SERVICE_UNAVAILABLE: uint16 503,
    GATEWAY_TIME_OUT: uint16 504,
    HTTP_VERSION_NOT_SUPPORTED: uint16 505

string = 
  repeat 
    0x20..0x7E 

bytes = 
  repeat 
    0x00..0xFF 

uint16 = 
  bits 16 

uint32 = 
  bits 32 

crlf = 
  0x0D 0x0A 

colon = 
  0x3A 

space = 
  0x20