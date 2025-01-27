// HTTP/1.1 Protocol Specification
module HTTP-1.1

// Basic types
type Digit = '0'-'9'
type Alpha = 'a'-'z' | 'A'-'Z'
type AlphaNum = Alpha | Digit
type Unreserved = Alpha | Digit | '-' | '.' | '_' | '~'
type SubDelims = '!' | '$' | '&' | ''' | '(' | ')' | '*' | '+' | ',' | ';' | '='

// Whitespace and Control Characters
type SP = ' '
type CRLF = '\r\n'
type WSP = SP | '\t'
type VCHAR = '\x21'-'\x7E'

// Header Field Parsing
type HeaderName = (Alpha | Digit | '-')+
type HeaderValue = VCHAR*

// Request Line Components
type Method = 'GET' | 'POST' | 'PUT' | 'DELETE' | 'HEAD' | 'OPTIONS' | 'TRACE' | 'CONNECT'
type AbsolutePath = '/' (Unreserved | SubDelims | ':' | '@')*
type HTTPVersion = 'HTTP/1.1'

// Request Line Structure
type RequestLine = 
    Method SP 
    AbsolutePath SP 
    HTTPVersion 
    CRLF

// Header Field
type HeaderField = 
    HeaderName ':' SP HeaderValue CRLF

// Message Body (optional)
type MessageBody = VCHAR*

// Complete HTTP Request
type HTTPRequest = 
    RequestLine 
    HeaderField* 
    CRLF 
    MessageBody?