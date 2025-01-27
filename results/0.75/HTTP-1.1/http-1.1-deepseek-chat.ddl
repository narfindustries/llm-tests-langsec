HTTP_1_1 = {
  start_line: StartLine,
  headers: Headers,
  body: Body
};

StartLine = {
  method: Method,
  space1: " ",
  request_target: RequestTarget,
  space2: " ",
  http_version: HTTPVersion,
  crlf: "\r\n"
};

Method = /[A-Z]+/;

RequestTarget = /[^ ]+/;

HTTPVersion = "HTTP/" Digit "." Digit;

Digit = /[0-9]/;

Headers = { header: Header, headers: Headers } | crlf: "\r\n";

Header = {
  field_name: FieldName,
  colon: ":",
  optional_whitespace: /[ \t]*/,
  field_value: FieldValue,
  crlf: "\r\n"
};

FieldName = /[^:\r\n]+/;

FieldValue = /[^\r\n]*/;

Body = /.*/;