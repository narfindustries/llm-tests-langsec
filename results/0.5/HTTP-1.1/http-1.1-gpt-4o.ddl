HTTP_Message : type = HTTP_Request | HTTP_Response;

HTTP_Request : type = struct {
    method: HTTP_Method,
    SP: SP,
    request_uri: URI,
    SP: SP,
    version: HTTP_Version,
    CRLF: CRLF,
    headers: HTTP_Headers,
    CRLF: CRLF,
    body: optional<HTTP_Body>
};

HTTP_Response : type = struct {
    version: HTTP_Version,
    SP: SP,
    status_code: Status_Code,
    SP: SP,
    reason_phrase: Reason_Phrase,
    CRLF: CRLF,
    headers: HTTP_Headers,
    CRLF: CRLF,
    body: optional<HTTP_Body>
};

HTTP_Method : type = enum {
    "OPTIONS",
    "GET",
    "HEAD",
    "POST",
    "PUT",
    "DELETE",
    "TRACE",
    "CONNECT"
};

URI : type = string;  # Simplified for brevity

HTTP_Version : type = "HTTP/1.1";

Status_Code : type = enum {
    "100", "101", "200", "201", "202", "203", "204", "205", "206",
    "300", "301", "302", "303", "304", "305", "307",
    "400", "401", "402", "403", "404", "405", "406", "407", "408", "409", "410", "411", "412", "413", "414", "415", "416", "417",
    "500", "501", "502", "503", "504", "505"
};

Reason_Phrase : type = string;  # Simplified for brevity

HTTP_Headers : type = list<HTTP_Header>;

HTTP_Header : type = General_Header | Request_Header | Response_Header | Entity_Header;

General_Header : type = Cache_Control | Connection | Date | Pragma | Trailer | Transfer_Encoding | Upgrade | Via | Warning;

Request_Header : type = Accept | Accept_Charset | Accept_Encoding | Accept_Language | Authorization | Expect | From | Host |
                 If_Match | If_Modified_Since | If_None_Match | If_Range | If_Unmodified_Since | Max_Forwards |
                 Proxy_Authorization | Range | Referer | TE | User_Agent;

Response_Header : type = Accept_Ranges | Age | ETag | Location | Proxy_Authenticate | Retry_After | Server | Vary | WWW_Authenticate;

Entity_Header : type = Allow | Content_Encoding | Content_Language | Content_Length | Content_Location | Content_MD5 |
                Content_Range | Content_Type | Expires | Last_Modified;

Cache_Control : type = struct {
    "Cache-Control": string,
    SP: SP,
    directive: Cache_Directive
};

Cache_Directive : type = enum {
    "no-cache", "no-store", "max-age=", "public", "private", "must-revalidate", "proxy-revalidate", "max-stale=", "min-fresh=", "no-transform", "only-if-cached"
};

Connection : type = struct {
    "Connection": string,
    SP: SP,
    option: Connection_Option
};

Connection_Option : type = enum {
    "keep-alive", "close", token
};

Date : type = struct {
    "Date": string,
    SP: SP,
    date: HTTP_Date
};

HTTP_Date : type = string;  # Simplified for brevity

Pragma : type = struct {
    "Pragma": string,
    SP: SP,
    directive: Pragma_Directive
};

Pragma_Directive : type = enum {
    "no-cache", token
};

Trailer : type = struct {
    "Trailer": string,
    SP: SP,
    headers: list<Header_Name>
};

Transfer_Encoding : type = struct {
    "Transfer-Encoding": string,
    SP: SP,
    coding: Transfer_Coding
};

Transfer_Coding : type = enum {
    "chunked", "compress", "deflate", "gzip", transfer_extension
};

Upgrade : type = struct {
    "Upgrade": string,
    SP: SP,
    products: list<Product>
};

Via : type = struct {
    "Via": string,
    SP: SP,
    received: list<Via_Received>
};

Via_Received : type = struct {
    version: HTTP_Version,
    SP: SP,
    host: Host,
    optional<SP>: SP,
    optional<comment>: comment
};

Warning : type = struct {
    "Warning": string,
    SP: SP,
    values: list<Warning_Value>
};

Warning_Value : type = struct {
    code: integer,
    SP: SP,
    host: Host,
    SP: SP,
    text: quoted_string
};

Accept : type = struct {
    "Accept": string,
    SP: SP,
    ranges: list<Media_Range>
};

Media_Range : type = struct {
    type: string,
    "/": string,
    subtype: string,
    optional<parameters>: parameters
};

Accept_Charset : type = struct {
    "Accept-Charset": string,
    SP: SP,
    charsets: list<Charset>
};

Charset : type = enum {
    token, "*"
};

Accept_Encoding : type = struct {
    "Accept-Encoding": string,
    SP: SP,
    codings: list<Coding>
};

Coding : type = enum {
    "gzip", "compress", "deflate", "identity", "*"
};

Accept_Language : type = struct {
    "Accept-Language": string,
    SP: SP,
    languages: list<Language_Range>
};

Language_Range : type = struct {
    primary: string,
    optional<"-">: "-",
    optional<subtag>: string
};

Authorization : type = struct {
    "Authorization": string,
    SP: SP,
    credentials: credentials
};

Expect : type = struct {
    "Expect": string,
    SP: SP,
    expectations: list<Expectation>
};

Expectation : type = enum {
    "100-continue", token "=" (token | quoted_string)
};

From : type = struct {
    "From": string,
    SP: SP,
    mailbox: mailbox
};

Host : type = struct {
    "Host": string,
    SP: SP,
    host: host,
    optional<":" port>: ":" port
};

If_Match : type = struct {
    "If-Match": string,
    SP: SP,
    tags: "*" | list<Entity_Tag>
};

If_Modified_Since : type = struct {
    "If-Modified-Since": string,
    SP: SP,
    date: HTTP_Date
};

If_None_Match : type = struct {
    "If-None-Match": string,
    SP: SP,
    tags: "*" | list<Entity_Tag>
};

If_Range : type = struct {
    "If-Range": string,
    SP: SP,
    entity: HTTP_Date | Entity_Tag
};

If_Unmodified_Since : type = struct {
    "If-Unmodified-Since": string,
    SP: SP,
    date: HTTP_Date
};

Max_Forwards : type = struct {
    "Max-Forwards": string,
    SP: SP,
    count: integer
};

Proxy_Authorization : type = struct {
    "Proxy-Authorization": string,
    SP: SP,
    credentials: credentials
};

Range : type = struct {
    "Range": string,
    SP: SP,
    specifier: ranges_specifier
};

ranges_specifier : type = struct {
    unit: "bytes",
    "=": string,
    set: byte_range_set
};

byte_range_set : type = list<byte_range_spec | suffix_byte_range_spec>;

byte_range_spec : type = struct {
    first: first_byte_pos,
    "-": string,
    optional<last>: last_byte_pos
};

suffix_byte_range_spec : type = struct {
    "-": string,
    length: suffix_length
};

Referer : type = struct {
    "Referer": string,
    SP: SP,
    uri: absoluteURI | relativeURI
};

TE : type = struct {
    "TE": string,
    SP: SP,
    codings: list<TE_Transfer_Coding>
};

TE_Transfer_Coding : type = struct {
    coding: transfer_coding,
    optional<";" "q" "=" qvalue>: ";" "q" "=" qvalue
};

User_Agent : type = struct {
    "User-Agent": string,
    SP: SP,
    products: list<product>
};

Accept_Ranges : type = struct {
    "Accept-Ranges": string,
    SP: SP,
    unit: "none" | range_unit
};

range_unit : type = bytes_unit | other_range_unit;

bytes_unit : type = "bytes";

Age : type = struct {
    "Age": string,
    SP: SP,
    seconds: delta_seconds
};

ETag : type = struct {
    "ETag": string,
    SP: SP,
    tag: entity_tag
};

Location : type = struct {
    "Location": string,
    SP: SP,
    uri: URI_reference | absoluteURI
};

Proxy_Authenticate : type = struct {
    "Proxy-Authenticate": string,
    SP: SP,
    challenge: challenge
};

Retry_After : type = struct {
    "Retry-After": string,
    SP: SP,
    time: HTTP_Date | delta_seconds
};

Server : type = struct {
    "Server": string,
    SP: SP,
    products: list<product>
};

Vary : type = struct {
    "Vary": string,
    SP: SP,
    headers: "*" | list<Header_Name>
};

WWW_Authenticate : type = struct {
    "WWW-Authenticate": string,
    SP: SP,
    challenge: challenge
};

Allow : type = struct {
    "Allow": string,
    SP: SP,
    methods: list<Method>
};

Content_Encoding : type = struct {
    "Content-Encoding": string,
    SP: SP,
    codings: list<Content_Coding>
};

Content_Coding : type = enum {
    "gzip", "compress", "deflate", "identity"
};

Content_Language : type = struct {
    "Content-Language": string,
    SP: SP,
    languages: list<Language_Tag>
};

Language_Tag : type = string;  # Simplified for brevity

Content_Length : type = struct {
    "Content-Length": string,
    SP: SP,
    length: 1*DIGIT
};

Content_Location : type = struct {
    "Content-Location": string,
    SP: SP,
    uri: URI_reference | absoluteURI
};

Content_MD5 : type = struct {
    "Content-MD5": string,
    SP: SP,
    digest: md5_digest
};

Content_Range : type = struct {
    "Content-Range": string,
    SP: SP,
    spec: content_range_spec
};

content_range_spec : type = struct {
    unit: bytes_unit,
    SP: SP,
    range: byte_range_resp_spec | unsatisfied_range
};

byte_range_resp_spec : type = struct {
    first: first_byte_pos,
    "-": string,
    last: last_byte_pos,
    "/": string,
    length: instance_length
};

unsatisfied_range : type = struct {
    "*": string,
    "/": string,
    length: instance_length
};

instance_length : type = 1*DIGIT;

Content_Type : type = struct {
    "Content-Type": string,
    SP: SP,
    type: media_type
};

media_type : type = struct {
    type: string,
    "/": string,
    subtype: string,
    optional<parameters>: parameters
};

Expires : type = struct {
    "Expires": string,
    SP: SP,
    date: HTTP_Date
};

Last_Modified : type = struct {
    "Last-Modified": string,
    SP: SP,
    date: HTTP_Date
};

Header_Name : type = token;

token : type = 1*tchar;

tchar : type = enum {
    "!", "#", "$", "%", "&", "'", "*", "+", "-", ".", "^", "_", "`", "|", "~", DIGIT, ALPHA
};

SP : type = " ";

CRLF : type = "\r\n";

HTTP_Body : type = bytes;  # Simplified for brevity

product : type = struct {
    name: token,
    optional<"/" version>: "/" product_version
};

product_version : type = token;

comment : type = struct {
    "(": string,
    text: *(ctext | quoted_pair | comment),
    ")": string
};

ctext : type = <any TEXT excluding "(" and ")">;

quoted_string : type = struct {
    DQUOTE: "\"",
    text: *(qdtext | quoted_pair),
    DQUOTE: "\""
};

qdtext : type = <any TEXT excluding DQUOTE and "\"">;

quoted_pair : type = struct {
    backslash: "\\",
    char: CHAR
};

CHAR : type = <any US-ASCII character (octets 0 - 127)>;

DIGIT : type = <any US-ASCII digit "0".."9">;

ALPHA : type = <any US-ASCII alphabetic character "A".."Z" or "a".."z">;

RWS : type = *(SP | HTAB);

HTAB : type = "\t";

bytes : type = *OCTET;

OCTET : type = <any 8-bit byte value>;

integer : type = 1*DIGIT;