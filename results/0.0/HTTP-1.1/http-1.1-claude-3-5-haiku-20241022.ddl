type HttpVersion = enum {
    HTTP_1_0,
    HTTP_1_1
}

type HttpMethod = enum {
    GET,
    POST,
    HEAD,
    PUT,
    DELETE,
    TRACE,
    OPTIONS,
    CONNECT
}

type StatusCode = enum {
    CONTINUE = 100,
    OK = 200,
    CREATED = 201,
    NO_CONTENT = 204,
    MOVED_PERMANENTLY = 301,
    BAD_REQUEST = 400,
    UNAUTHORIZED = 401,
    FORBIDDEN = 403,
    NOT_FOUND = 404,
    INTERNAL_SERVER_ERROR = 500
}

type HeaderField = record {
    name: string,
    value: string
}

type RequestUri = variant {
    AbsoluteUri(string),
    AbsolutePath(string),
    Authority(string),
    Asterisk
}

type HttpRequest = record {
    method: HttpMethod,
    uri: RequestUri,
    version: HttpVersion,
    headers: list<HeaderField>,
    body: optional<bytes>
}

type HttpResponse = record {
    version: HttpVersion,
    status: StatusCode,
    headers: list<HeaderField>,
    body: optional<bytes>
}

type OptionalRequestHeaders = record {
    accept: optional<string>,
    accept_charset: optional<string>,
    accept_encoding: optional<string>,
    accept_language: optional<string>,
    authorization: optional<string>,
    expect: optional<string>,
    from: optional<string>,
    host: string,
    if_match: optional<string>,
    if_modified_since: optional<string>,
    if_none_match: optional<string>,
    if_range: optional<string>,
    if_unmodified_since: optional<string>,
    max_forwards: optional<int>,
    proxy_authorization: optional<string>,
    range: optional<string>,
    referer: optional<string>,
    te: optional<string>,
    user_agent: optional<string>
}

type OptionalResponseHeaders = record {
    accept_ranges: optional<string>,
    age: optional<int>,
    etag: optional<string>,
    location: optional<string>,
    proxy_authenticate: optional<string>,
    retry_after: optional<string>,
    server: optional<string>,
    vary: optional<string>,
    www_authenticate: optional<string>
}