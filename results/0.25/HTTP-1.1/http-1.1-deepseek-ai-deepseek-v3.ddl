HTTP_Message = Request | Response;

Request = {
  Request-Line,
  *(General-Header | Request-Header),
  [Entity-Header],
  [CRLF Body]
};

Response = {
  Status-Line,
  *(General-Header | Response-Header),
  [Entity-Header],
  [CRLF Body]
};

Request-Line = Method SP Request-URI SP HTTP-Version CRLF;

Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF;

Method = "GET" | "POST" | "PUT" | "DELETE" | "HEAD" | "OPTIONS" | "TRACE" | "CONNECT";

Request-URI = "*" | absoluteURI | abs_path;

HTTP-Version = "HTTP/" DIGIT "." DIGIT;

Status-Code = 3DIGIT;

Reason-Phrase = *( HTAB | SP | VCHAR | obs-text );

General-Header = Cache-Control
                | Connection
                | Date
                | Pragma
                | Trailer
                | Transfer-Encoding
                | Upgrade
                | Via
                | Warning;

Request-Header = Accept
                | Accept-Charset
                | Accept-Encoding
                | Accept-Language
                | Authorization
                | Expect
                | From
                | Host
                | If-Match
                | If-Modified-Since
                | If-None-Match
                | If-Range
                | If-Unmodified-Since
                | Max-Forwards
                | Proxy-Authorization
                | Range
                | Referer
                | TE
                | User-Agent;

Response-Header = Accept-Ranges
                | Age
                | ETag
                | Location
                | Proxy-Authenticate
                | Retry-After
                | Server
                | Vary
                | WWW-Authenticate;

Entity-Header = Allow
              | Content-Encoding
              | Content-Language
              | Content-Length
              | Content-Location
              | Content-MD5
              | Content-Range
              | Content-Type
              | Expires
              | Last-Modified;

Cache-Control = "Cache-Control" ":" 1#cache-directive;

Connection = "Connection" ":" 1#(token | "close");

Date = "Date" ":" HTTP-date;

Pragma = "Pragma" ":" 1#pragma-directive;

Trailer = "Trailer" ":" 1#field-name;

Transfer-Encoding = "Transfer-Encoding" ":" 1#transfer-coding;

Upgrade = "Upgrade" ":" 1#product;

Via = "Via" ":" 1#(received-protocol received-by [comment]);

Warning = "Warning" ":" 1#warning-value;

Accept = "Accept" ":" 1#(media-range [accept-params]);

Accept-Charset = "Accept-Charset" ":" 1#(charset [ ";" "q" "=" qvalue ]);

Accept-Encoding = "Accept-Encoding" ":" 1#(codings [ ";" "q" "=" qvalue ]);

Accept-Language = "Accept-Language" ":" 1#(language-range [ ";" "q" "=" qvalue ]);

Authorization = "Authorization" ":" credentials;

Expect = "Expect" ":" 1#expectation;

From = "From" ":" mailbox;

Host = "Host" ":" host [ ":" port ];

If-Match = "If-Match" ":" ( "*" | 1#entity-tag );

If-Modified-Since = "If-Modified-Since" ":" HTTP-date;

If-None-Match = "If-None-Match" ":" ( "*" | 1#entity-tag );

If-Range = "If-Range" ":" ( entity-tag | HTTP-date );

If-Unmodified-Since = "If-Unmodified-Since" ":" HTTP-date;

Max-Forwards = "Max-Forwards" ":" 1*DIGIT;

Proxy-Authorization = "Proxy-Authorization" ":" credentials;

Range = "Range" ":" ranges-specifier;

Referer = "Referer" ":" absoluteURI | relativeURI;

TE = "TE" ":" 1#( t-codings [ ";" "q" "=" qvalue ]);

User-Agent = "User-Agent" ":" 1*( product | comment );

Accept-Ranges = "Accept-Ranges" ":" acceptable-ranges;

Age = "Age" ":" delta-seconds;

ETag = "ETag" ":" entity-tag;

Location = "Location" ":" absoluteURI;

Proxy-Authenticate = "Proxy-Authenticate" ":" 1#challenge;

Retry-After = "Retry-After" ":" ( HTTP-date | delta-seconds );

Server = "Server" ":" 1*( product | comment );

Vary = "Vary" ":" ( "*" | 1#field-name );

WWW-Authenticate = "WWW-Authenticate" ":" 1#challenge;

Allow = "Allow" ":" 1#method;

Content-Encoding = "Content-Encoding" ":" 1#content-coding;

Content-Language = "Content-Language" ":" 1#language-tag;

Content-Length = "Content-Length" ":" 1*DIGIT;

Content-Location = "Content-Location" ":" ( absoluteURI | relativeURI );

Content-MD5 = "Content-MD5" ":" md5-digest;

Content-Range = "Content-Range" ":" content-range-spec;

Content-Type = "Content-Type" ":" media-type;

Expires = "Expires" ":" HTTP-date;

Last-Modified = "Last-Modified" ":" HTTP-date;

Body = *OCTET;

CRLF = "\r\n";

SP = " ";

HTAB = "\t";

DIGIT = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";

VCHAR = %x21-7E;

obs-text = %x80-FF;

absoluteURI = scheme ":" hier-part [ "?" query ];

abs_path = "/" path-segments;

scheme = ALPHA *( ALPHA | DIGIT | "+" | "-" | "." );

hier-part = "//" authority path-abempty | path-absolute | path-rootless | path-empty;

path-segments = segment *( "/" segment );

segment = *pchar;

pchar = unreserved | pct-encoded | sub-delims | ":" | "@";

unreserved = ALPHA | DIGIT | "-" | "." | "_" | "~";

pct-encoded = "%" HEXDIG HEXDIG;

sub-delims = "!" | "$" | "&" | "'" | "(" | ")" | "*" | "+" | "," | ";" | "=";

query = *( pchar | "/" | "?" );

fragment = *( pchar | "/" | "?" );

ALPHA = %x41-5A | %x61-7A;

HEXDIG = DIGIT | "A" | "B" | "C" | "D" | "E" | "F";

OCTET = %x00-FF;

token = 1*tchar;

tchar = "!" | "#" | "$" | "%" | "&" | "'" | "*" | "+" | "-" | "." | "^" | "_" | "`" | "|" | "~" | DIGIT | ALPHA;

comment = "(" *( ctext | quoted-pair | comment ) ")";

ctext = HTAB | SP | %x21-27 | %x2A-5B | %x5D-7E | obs-text;

quoted-pair = "\" ( HTAB | SP | VCHAR | obs-text );

field-name = token;

field-value = *( field-content | obs-fold );

field-content = field-vchar [ 1*( SP | HTAB ) field-vchar ];

field-vchar = VCHAR | obs-text;

obs-fold = CRLF 1*( SP | HTAB );

media-range = ( "*/*" | type "/*" | type "/" subtype ) *( ";" parameter );

accept-params = ";" "q" "=" qvalue *( accept-ext );

accept-ext = ";" token [ "=" ( token | quoted-string ) ];

qvalue = ( "0" [ "." 0*3DIGIT ] ) | ( "1" [ "." 0*3("0") ] );

charset = token;

codings = content-coding | "identity" | "*";

content-coding = token;

language-range = ( 1*8ALPHA *( "-" 1*8ALPHA ) ) | "*";

mailbox = addr-spec;

addr-spec = local-part "@" domain;

local-part = dot-atom | quoted-string;

domain = dot-atom | domain-literal;

dot-atom = 1*atext *( "." 1*atext );

atext = ALPHA | DIGIT | "!" | "#" | "$" | "%" | "&" | "'" | "*" | "+" | "-" | "/" | "=" | "?" | "^" | "_" | "`" |