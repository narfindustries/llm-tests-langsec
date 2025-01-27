def Main = HTTP_message

def HTTP_message = Request | Response

def Request = {
  request_line = RequestLine
  headers = Many0 MessageHeader
  CRLF
  body = MessageBody?
}

def Response = {
  status_line = StatusLine
  headers = Many0 MessageHeader
  CRLF
  body = MessageBody?
}

def RequestLine = {
  method = Token
  SP
  request_uri = RequestURI
  SP
  version = HTTPVersion
  CRLF
}

def StatusLine = {
  version = HTTPVersion
  SP
  status_code = StatusCode
  SP
  reason = ReasonPhrase
  CRLF
}

def HTTPVersion = {
  "HTTP/"
  major = DIGIT
  "."
  minor = DIGIT
}

def StatusCode = DIGIT DIGIT DIGIT

def ReasonPhrase = Many0 (TEXT | SP)

def MessageHeader = {
  field_name = Token
  ":"
  SP
  field_value = Many1 (TEXT | SP)
  CRLF
}

def MessageBody = Many1 OCTET

def RequestURI = Many1 (ALPHA | DIGIT | '/' | '.' | '-' | '_')

def Token = Many1 (ALPHA | DIGIT | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_' | '`' | '|' | '~')

def CRLF = '\r' '\n'
def SP = ' '
def DIGIT = '0'..'9'
def ALPHA = 'a'..'z' | 'A'..'Z'
def TEXT = ALPHA | DIGIT | SP | PUNCT
def PUNCT = '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~'
def OCTET = UInt8