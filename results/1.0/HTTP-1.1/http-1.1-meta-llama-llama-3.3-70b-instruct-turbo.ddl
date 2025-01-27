domain http {
  version "1.1";

  type RequestLine = struct {
    method: string(3..7); // allowed methods: GET, POST, PUT, DELETE, etc.
    url: string;
    http_version: string = "HTTP/1.1";
  };

  type StatusLine = struct {
    http_version: string = "HTTP/1.1";
    status_code: integer(100..599);
    reason_phrase: string;
  };

  type HeaderField = struct {
    field_name: string;
    field_value: string;
  };

  type Message = struct {
    start_line: choice {
      request: RequestLine;
      response: StatusLine;
    };
    headers: array of HeaderField;
    body: optional bytes;
  };
}

grammar http_grammar {
  rule RequestLine: 
    method: string(3..7) >> 
    url: string >> 
    http_version: "HTTP/1.1" >> 
    "\r\n";

  rule StatusLine: 
    "HTTP/1.1" >> 
    status_code: integer(100..599) >> 
    reason_phrase: string >> 
    "\r\n";

  rule HeaderField: 
    field_name: string >> 
    ": " >> 
    field_value: string >> 
    "\r\n";

  rule Message: 
    start_line: choice {
      request: RequestLine;
      response: StatusLine;
    } >> 
    headers: array of HeaderField >> 
    body: optional bytes;

  start rule Message;
}