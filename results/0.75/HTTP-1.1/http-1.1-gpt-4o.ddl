module HttpMessage {
    Message = Request | Response;

    Request {
        method: Token;
        uri: Token;
        version: HttpVersion;
        crlf_line: "\r\n";
        headers: Headers;
        crlf_body: "\r\n";
        body: OptionalBody;
    }

    Response {
        version: HttpVersion;
        status_code: Token;
        reason_phrase: StringTerminatedBy<"\r\n">;
        headers: Headers;
        crlf_body: "\r\n";
        body: OptionalBody;
    }

    HttpVersion {
        prefix: "HTTP/";
        major: UInt8;
        dot: ".";
        minor: UInt8;
    }

    Headers {
        entries: Header*;
    }

    Header {
        name: Token;
        sep: ": ";
        value: StringTerminatedBy<"\r\n">;
    }

    OptionalBody {
        has_body: not Peek<"\r\n">;
        body: has_body ? DataUntilEof : null;
    }

    Token {
        value: StringUntil<" " | "\r\n">;
    }

    StringTerminatedBy<term: string> {
        value: StringUntil<term>;
        term: term;
    }

    StringUntil<end: string> {
        chars: (UInt8Until<end>)*;
    }

    UInt8Until<end: string> {
        value: UInt8;
        not_end: value != end;
    }

    DataUntilEof {
        bytes: UInt8*;
    }
}