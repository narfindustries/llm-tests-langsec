grammar HTTP;

import std.ascii;

type Request = struct {
    MethodLine methodLine;
    Headers headers @separator(EndOfLine);
    optional<string> body @requires(hasBody(headers));
};

type MethodLine = struct {
    string method;
    string uri;
    string version;
};

type Headers = struct {
    string key;
    string value;
};

type Response = struct {
    StatusLine statusLine;
    Headers headers @separator(EndOfLine);
    optional<string> body @requires(hasBody(headers));
};

type StatusLine = struct {
    string version;
    uint16 statusCode;
    string reasonPhrase;
};

predicate hasBody(Headers[] headers) -> bool {
    foreach(header in headers) {
        if (header.key == "Content-Length" && parseUInt(header.value) > 0) {
            return true;
        }
    }
    return false;
}

function parseUInt(string s) -> uint {
    return uint(parse(s));
}