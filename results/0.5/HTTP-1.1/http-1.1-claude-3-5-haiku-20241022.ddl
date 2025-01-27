format HTTP-1.1 {
    // Header definition
    type Header = struct {
        name: string<1..256>,
        value: string<1..4096>
    }

    // Request line components
    type Method = enum {
        GET, POST, PUT, DELETE, HEAD, OPTIONS, PATCH, TRACE, CONNECT
    }

    type RequestURI = string<1..2048>

    type HTTPVersion = enum {
        HTTP_1_0, 
        HTTP_1_1
    }

    // Request structure
    type Request = struct {
        method: Method,
        uri: RequestURI,
        version: HTTPVersion,
        headers: list<Header>,
        body: optional<bytes<0..65536>>
    }

    // Response structure 
    type StatusCode = uint16 where value >= 100 && value < 600

    type Response = struct {
        version: HTTPVersion,
        status: StatusCode,
        headers: list<Header>,
        body: optional<bytes<0..1048576>>
    }

    // Main parsing rules
    rule request: Request = {
        method: parse_method(),
        uri: parse_uri(),
        version: parse_version(),
        headers: parse_headers(),
        body: optional_body()
    }

    rule response: Response = {
        version: parse_version(),
        status: parse_status_code(),
        headers: parse_headers(),
        body: optional_body()
    }

    // Helper parsing functions
    func parse_method(): Method = {
        match input {
            "GET" => Method.GET,
            "POST" => Method.POST,
            "PUT" => Method.PUT,
            "DELETE" => Method.DELETE,
            "HEAD" => Method.HEAD,
            "OPTIONS" => Method.OPTIONS,
            "PATCH" => Method.PATCH,
            "TRACE" => Method.TRACE,
            "CONNECT" => Method.CONNECT
        }
    }

    func parse_uri(): RequestURI = {
        take_while(is_valid_uri_char)
    }

    func parse_version(): HTTPVersion = {
        match input {
            "HTTP/1.0" => HTTPVersion.HTTP_1_0,
            "HTTP/1.1" => HTTPVersion.HTTP_1_1
        }
    }

    func parse_headers(): list<Header> = {
        many(parse_header())
    }

    func parse_header(): Header = {
        name: take_while(is_header_name_char),
        value: take_while(is_header_value_char)
    }

    func parse_status_code(): StatusCode = {
        take_uint16()
    }

    func optional_body(): optional<bytes<0..65536>> = {
        optional(take_bytes())
    }

    // Character validation helpers
    func is_valid_uri_char(c: char): bool = {
        (c >= 'a' && c <= 'z') || 
        (c >= 'A' && c <= 'Z') || 
        (c >= '0' && c <= '9') || 
        c == '/' || c == '.' || c == '-' || c == '_' || c == '?' || c == '='
    }

    func is_header_name_char(c: char): bool = {
        (c >= 'a' && c <= 'z') || 
        (c >= 'A' && c <= 'Z') || 
        (c >= '0' && c <= '9') || 
        c == '-' || c == '_'
    }

    func is_header_value_char(c: char): bool = {
        c >= 32 && c <= 126
    }
}