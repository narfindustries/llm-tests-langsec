HTTP/1.1 {
    start = Request | Response

    Request = {
        method: Method
        sp
        uri: URI
        sp
        version: Version
        crlf
        headers: Headers
        crlf
        body: Body?
    }

    Response = {
        version: Version
        sp
        status: Status
        sp
        reason: Reason
        crlf
        headers: Headers
        crlf
        body: Body?
    }

    Method = "GET" | "POST" | "PUT" | "DELETE" | "HEAD" | "OPTIONS" | "PATCH"

    URI = <string> {
        until = " "
    }

    Version = "HTTP/1.1"

    Status = <uint8> {
        until = " "
    }

    Reason = <string> {
        until = "\r\n"
    }

    Headers = {
        header: Header*
    }

    Header = {
        name: HeaderName
        ":"
        sp
        value: HeaderValue
        crlf
    }

    HeaderName = <string> {
        until = ":"
    }

    HeaderValue = <string> {
        until = "\r\n"
    }

    Body = <bytes> {
        until = eof
    }

    sp = " "
    crlf = "\r\n"
}