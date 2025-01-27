struct HTTPRequest {
  method: Method;
  uri: URI;
  version: Version;
  headers: Headers;
  body: Body;
}

struct Method {
  value: /[A-Z]+/;
}

struct URI {
  value: /[^\s]+/;
}

struct Version {
  value: /HTTP\/\d\.\d/;
}

struct Headers {
  entries: HeaderEntry[];
}

struct HeaderEntry {
  name: /[^:\r\n]+/;
  _: ": ";
  value: /[^\r\n]+/;
  _: "\r\n";
}

struct Body {
  content: /.*/;
}

struct HTTPResponse {
  version: Version;
  status: Status;
  headers: Headers;
  body: Body;
}

struct Status {
  code: /[0-9]{3}/;
  reason: /[^\r\n]+/;
  _: "\r\n";
}