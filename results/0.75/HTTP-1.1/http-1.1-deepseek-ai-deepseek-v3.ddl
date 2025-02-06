HTTPMessage = Request | Response;

Request = {
    request_line: RequestLine,
    headers: Headers,
    body: Body?
};

Response = {
    status_line: StatusLine,
    headers: Headers,
    body: Body?
};

RequestLine = {
    method: Method,
    request_uri: URI,
    http_version: HTTPVersion
};

StatusLine = {
    http_version: HTTPVersion,
    status_code: StatusCode,
    reason_phrase: ReasonPhrase
};

Method = "GET" | "POST" | "PUT" | "DELETE" | "HEAD" | "OPTIONS" | "TRACE" | "CONNECT" | "PATCH";

URI = String;

HTTPVersion = "HTTP/1.1";

StatusCode = Number;

ReasonPhrase = String;

Headers = {
    "Accept": String?,
    "Accept-Charset": String?,
    "Accept-Encoding": String?,
    "Accept-Language": String?,
    "Authorization": String?,
    "Cache-Control": String?,
    "Connection": String?,
    "Content-Encoding": String?,
    "Content-Language": String?,
    "Content-Length": String?,
    "Content-Location": String?,
    "Content-MD5": String?,
    "Content-Range": String?,
    "Content-Type": String?,
    "Date": String?,
    "ETag": String?,
    "Expect": String?,
    "Expires": String?,
    "From": String?,
    "Host": String?,
    "If-Match": String?,
    "If-Modified-Since": String?,
    "If-None-Match": String?,
    "If-Range": String?,
    "If-Unmodified-Since": String?,
    "Last-Modified": String?,
    "Location": String?,
    "Max-Forwards": String?,
    "Pragma": String?,
    "Proxy-Authenticate": String?,
    "Proxy-Authorization": String?,
    "Range": String?,
    "Referer": String?,
    "Retry-After": String?,
    "Server": String?,
    "TE": String?,
    "Trailer": String?,
    "Transfer-Encoding": String?,
    "Upgrade": String?,
    "User-Agent": String?,
    "Vary": String?,
    "Via": String?,
    "Warning": String?,
    "WWW-Authenticate": String?
};

Body = String;