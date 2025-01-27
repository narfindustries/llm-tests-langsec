grammar http_1_1 {
  rule request_line = 
    method: token("GET" | "POST" | "PUT" | "DELETE" | "HEAD" | "OPTIONS" | "CONNECT" | "PATCH") 
    SPACE 
    request_target: token("/" | relative_uri) 
    SPACE 
    http_version: token("HTTP/1.1") CRLF;

  rule response_line = 
    http_version: token("HTTP/1.1") 
    SPACE 
    status_code: uint8(100..599) 
    SPACE 
    reason_phrase: token(.*) CRLF;

  rule header_field = 
    field_name: token([!#$%&'*+\-./0-9A-Z^_`a-z~]+) 
    COLON 
    field_value: token(.*) CRLF;

  rule headers = 
    *(header_field);

  rule request = 
    request_line 
    headers;

  rule response = 
    response_line 
    headers;

  rule message = 
    request | response;

  rule relative_uri = 
    token([!#$%&'*+\-./0-9A-Z^_`a-z~]+);

  rule token = 
    token_value: .+ 

  rule uint8 = 
    uint8_value: [0-9]+ 

  rule CRLF = 
    "\r\n"

  rule COLON = 
    ":"

  rule SPACE = 
    " "
}