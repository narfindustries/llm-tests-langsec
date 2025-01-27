def Main = HTTP_message

def HTTP_message = Request | Response

def Request = {
  request_line = RequestLine
  headers = $Many(Header)
  CRLF
  body = MessageBody?
}

def Response = {
  status_line = StatusLine
  headers = $Many(Header)
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

def Header = {
  name = Token
  ':' SP?
  value = HeaderValue
  CRLF
}

def HeaderValue = $Many1($Choose(['-', '/', '.', '_', '~', '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=', ':', '@'] | ALPHA | DIGIT | SP))

def MessageBody = $Many1($Choose(VCHAR | SP | HTAB))

def HTTPVersion = {
  'HTTP/'
  major = DIGIT
  '.'
  minor = DIGIT
}

def StatusCode = DIGIT DIGIT DIGIT

def ReasonPhrase = $Many($Choose(['-', '.', ' '] | ALPHA))

def RequestURI = $Many1($Choose(['/', '-', '.', '_', '~', '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=', ':', '@'] | ALPHA | DIGIT))

def Token = $Many1($Choose(['-', '.', '_', '~', '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=', '@'] | ALPHA | DIGIT))

def DIGIT = $Range('0', '9')
def ALPHA = $Range('a', 'z') | $Range('A', 'Z')
def VCHAR = $Range('!', '~')
def SP = ' '
def HTAB = '\t'
def CRLF = '\r\n'