enum Version {
    HTTP_1_0,
    HTTP_1_1
}

enum Method {
    GET,
    POST,
    HEAD,
    PUT,
    DELETE,
    TRACE,
    OPTIONS,
    CONNECT
}

type StatusCode = Int where 100 <= value and value < 600

enum StatusClass {
    INFORMATIONAL,
    SUCCESSFUL,
    REDIRECTION,
    CLIENT_ERROR,
    SERVER_ERROR
}

record HeaderField {
    name: String,
    value: String
}

variant RequestURI {
    AbsoluteURI(uri: String),
    AbsolutePath(path: String),
    Authority(host: String),
    Asterisk
}

record Request {
    method: Method,
    uri: RequestURI,
    version: Version,
    headers: List<HeaderField>,
    body: Optional<Bytes>
}

record Response {
    version: Version,
    status_code: StatusCode,
    status_class: StatusClass,
    status_text: String,
    headers: List<HeaderField>,
    body: Optional<Bytes>
}

variant Message {
    RequestMessage(request: Request),
    ResponseMessage(response: Response)
}

parser HTTPParser {
    parse(input: Bytes) -> Message
}