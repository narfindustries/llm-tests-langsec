grammar HTTP {
    version = struct {
        major: u8;
        minor: u8;
    }

    method = enum {
        GET,
        POST,
        PUT,
        DELETE,
        HEAD,
        OPTIONS,
        TRACE,
        CONNECT,
        PATCH
    }

    header = struct {
        name: string;
        value: string;
        is_optional: bool;
    }

    request = struct {
        method: method;
        uri: string;
        version: version;
        headers: list<header>;
        body: option<bytes>;
    }

    response = struct {
        version: version;
        status_code: u16;
        status_text: string;
        headers: list<header>;
        body: option<bytes>;
    }

    message = variant {
        request_msg(request);
        response_msg(response);
    }

    parser http_parser {
        parse_request(input: bytes) -> request;
        parse_response(input: bytes) -> response;
        parse_message(input: bytes) -> message;
    }
}