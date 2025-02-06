module HTTP_1_1;

type SP = ' ';
type CRLF = '\r\n';
type token = /[A-Za-z0-9!#$%&'*+\\-^_`|~]+/;
type quoted_string = '"' (/[^\"]*/ -> String) '"';
type value = token | quoted_string;

type request_line = {
  method: token,
  _sp1: SP,
  request_URI: token,
  _sp2: SP,
  HTTP_Version: "HTTP/" DIGIT+ "." DIGIT,
  _crlf: CRLF
};

type response_line = {
  HTTP_Version: "HTTP/" DIGIT+ "." DIGIT,
  _sp1: SP,
  status_code: DIGIT{3},
  _sp2: SP,
  reason_phrase: /[^\\r\\n]*/,
  _crlf: CRLF
};

type header_field = {
  field_name: token,
  _: ":",
  field_value: (SP | token | quoted_string)+,
  _crlf: CRLF
};

type header_fields = [header_field]+;

type HTTP_message = {
  start_line: request_line | response_line,
  headers: header_fields,
  _crlf: CRLF,
  optional_body: /(.*)/ -> Bytes
};