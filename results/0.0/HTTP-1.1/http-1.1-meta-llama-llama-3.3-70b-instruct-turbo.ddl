domain http {
  import std.all;

  type Request = struct {
    method: string(3..7),
    url: string,
    http_version: string(8),
    headers: array(Header),
    body: bytes
  };

  type Header = struct {
    name: string,
    value: string
  };

  type Response = struct {
    http_version: string(8),
    status_code: uint16,
    reason_phrase: string,
    headers: array(Header),
    body: bytes
  };

  grammar RequestGrammar {
    entry_point: request;

    request: method, url, http_version, headers, body;
    method: "GET" | "POST" | "PUT" | "DELETE" | "HEAD" | "OPTIONS" | "CONNECT" | "PATCH";
    url: string;
    http_version: "HTTP/1.1" | "HTTP/1.0";
    headers: header*;
    header: name, ":", value, "\r\n";
    name: string;
    value: string;
    body: bytes;
  }

  grammar ResponseGrammar {
    entry_point: response;

    response: http_version, status_code, reason_phrase, headers, body;
    http_version: "HTTP/1.1" | "HTTP/1.0";
    status_code: uint16;
    reason_phrase: string;
    headers: header*;
    header: name, ":", value, "\r\n";
    name: string;
    value: string;
    body: bytes;
  }

  parser RequestParser {
    grammar: RequestGrammar;
  }

  parser ResponseParser {
    grammar: ResponseGrammar;
  }

  serializer RequestSerializer {
    grammar: RequestGrammar;
  }

  serializer ResponseSerializer {
    grammar: ResponseGrammar;
  }
}