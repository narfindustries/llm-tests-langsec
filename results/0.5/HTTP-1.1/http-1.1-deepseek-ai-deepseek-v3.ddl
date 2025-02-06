http_1_1 = {
  Request = {
    Request-Line = {
      Method = "GET" | "POST" | "PUT" | "DELETE" | "HEAD" | "OPTIONS" | "TRACE" | "CONNECT";
      SP = " ";
      Request-URI = *CHAR;
      SP = " ";
      HTTP-Version = "HTTP/1.1";
      CRLF = "\r\n";
    };
    *(General-Header | Request-Header | Entity-Header);
    CRLF = "\r\n";
    [Message-Body];
  };
  Response = {
    Status-Line = {
      HTTP-Version = "HTTP/1.1";
      SP = " ";
      Status-Code = 3DIGIT;
      SP = " ";
      Reason-Phrase = *CHAR;
      CRLF = "\r\n";
    };
    *(General-Header | Response-Header | Entity-Header);
    CRLF = "\r\n";
    [Message-Body];
  };
};

General-Header = {
  [Cache-Control = "Cache-Control: " *(Cache-Directive)];
  [Connection = "Connection: " *(Token)];
  [Date = "Date: " HTTP-Date];
  [Pragma = "Pragma: " *(Token)];
  [Trailer = "Trailer: " *(Field-Name)];
  [Transfer-Encoding = "Transfer-Encoding: " *(Transfer-Coding)];
  [Upgrade = "Upgrade: " *(Product)];
  [Via = "Via: " *(Received-By)];
  [Warning = "Warning: " *(Warning-Value)];
};

Request-Header = {
  [Accept = "Accept: " *(Media-Range)];
  [Accept-Charset = "Accept-Charset: " *(Charset)];
  [Accept-Encoding = "Accept-Encoding: " *(Content-Coding)];
  [Accept-Language = "Accept-Language: " *(Language-Tag)];
  [Authorization = "Authorization: " Credentials];
  [Expect = "Expect: " *(Expectation)];
  [From = "From: " Email];
  [Host = "Host: " Host-Value];
  [If-Match = "If-Match: " *(Entity-Tag)];
  [If-Modified-Since = "If-Modified-Since: " HTTP-Date];
  [If-None-Match = "If-None-Match: " *(Entity-Tag)];
  [If-Range = "If-Range: " (Entity-Tag | HTTP-Date)];
  [If-Unmodified-Since = "If-Unmodified-Since: " HTTP-Date];
  [Max-Forwards = "Max-Forwards: " 1*DIGIT];
  [Proxy-Authorization = "Proxy-Authorization: " Credentials];
  [Range = "Range: " *(Byte-Range-Spec)];
  [Referer = "Referer: " Absolute-URI];
  [TE = "TE: " *(Transfer-Coding)];
  [User-Agent = "User-Agent: " *(Product)];
};

Entity-Header = {
  [Allow = "Allow: " *(Method)];
  [Content-Encoding = "Content-Encoding: " *(Content-Coding)];
  [Content-Language = "Content-Language: " *(Language-Tag)];
  [Content-Length = "Content-Length: " 1*DIGIT];
  [Content-Location = "Content-Location: " (Absolute-URI | Relative-URI)];
  [Content-MD5 = "Content-MD5: " 1*HEXDIG];
  [Content-Range = "Content-Range: " Byte-Range-Spec];
  [Content-Type = "Content-Type: " Media-Type];
  [Expires = "Expires: " HTTP-Date];
  [Last-Modified = "Last-Modified: " HTTP-Date];
};

Response-Header = {
  [Accept-Ranges = "Accept-Ranges: " *(Range-Unit)];
  [Age = "Age: " 1*DIGIT];
  [ETag = "ETag: " Entity-Tag];
  [Location = "Location: " Absolute-URI];
  [Proxy-Authenticate = "Proxy-Authenticate: " *(Challenge)];
  [Retry-After = "Retry-After: " (HTTP-Date | Delta-Seconds)];
  [Server = "Server: " *(Product)];
  [Vary = "Vary: " *(Field-Name)];
  [WWW-Authenticate = "WWW-Authenticate: " *(Challenge)];
};

Message-Body = *OCTET;

Cache-Directive = Token ["=" (Token | Quoted-String)];
Transfer-Coding = Token [";" *(Parameter)];
Received-By = (Host [":" Port]) | Pseudonym;
Warning-Value = Warning-Code SP Warning-Agent SP Warning-Text SP [Warning-Date];
Media-Range = ( "*/*" | (Type "/" "*") | (Type "/" Subtype) ) [";" *(Parameter)];
Charset = Token;
Content-Coding = Token;
Language-Tag = Primary-Tag *( "-" Subtag );
Credentials = Token *( " " Token );
Expectation = Token ["=" (Token | Quoted-String)];
Host-Value = Host [":" Port];
Entity-Tag = [Weak] Opaque-Tag;
Byte-Range-Spec = 1*DIGIT "-" 1*DIGIT;
Absolute-URI = Scheme ":" *(Unreserved | Reserved | Escaped);
Relative-URI = *(Unreserved | Reserved | Escaped);
Product = Token ["/" Product-Version];
Method = Token;
Media-Type = Type "/" Subtype [";" *(Parameter)];
Range-Unit = Token;
Challenge = Auth-Scheme [" " *(Token | Quoted-String)];
Delta-Seconds = 1*DIGIT;
Field-Name = Token;
Opaque-Tag = Quoted-String;
Weak = "W/";
Quoted-String = DQUOTE *(qdtext | quoted-pair) DQUOTE;
qdtext = <any TEXT except <">>;
quoted-pair = "\" CHAR;
Parameter = Token ["=" (Token | Quoted-String)];
Scheme = ALPHA *(ALPHA | DIGIT | "+" | "-" | ".");
Unreserved = ALPHA | DIGIT | "-" | "." | "_" | "~";
Reserved = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | ",";
Escaped = "%" HEXDIG HEXDIG;
Auth-Scheme = Token;
Primary-Tag = 1*8ALPHA;
Subtag = 1*8ALPHA;
Product-Version = Token;
Type = Token;
Subtype = Token;
Host = *(Domain-Label ".") Top-Label;
Domain-Label = ALPHA | DIGIT | "-";
Top-Label = ALPHA | ALPHA *(ALPHA | DIGIT | "-");
Port = 1*DIGIT;
Pseudonym = Token;
Warning-Code = 3DIGIT;
Warning-Agent = Token | Quoted-String;
Warning-Text = Quoted-String;
Warning-Date = HTTP-Date;
HTTP-Date = <HTTP-date as defined in RFC 2616>;
CHAR = <any US-ASCII character (octets 0 - 127)>;
OCTET = <any 8-bit sequence of data>;
SP = <US-ASCII SP, space (32)>;
CRLF = <US-ASCII CR, carriage return (13)> <US-ASCII LF, linefeed (10)>;
DIGIT = <any US-ASCII digit (48-57)>;
HEXDIG = <any US-ASCII hexadecimal digit (48-57, 65-70, 97-102)>;
ALPHA = <any US-ASCII letter (65-90, 97-122)>;
DQUOTE = <US-ASCII double-quote mark (34)>;
TEXT = <any OCTET except CTLs, but including LWS>;
CTL = <any US-ASCII control character (octets 0 - 31) and DEL (127)>;
LWS = [CRLF] 1*(SP | HT);
HT = <US-ASCII horizontal-tab (9)>;
Token = 1*<any CHAR except CTLs or separators>;
separators = "(" | ")" | "<" | ">" | "@" | "," | ";" | ":" | "\" | <"> | "/" | "[" | "]" | "?" | "=" | "{" | "}" | SP | HT;
Email = <email address as defined in RFC 5322>;