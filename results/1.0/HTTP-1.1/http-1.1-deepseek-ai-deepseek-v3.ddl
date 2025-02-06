let HTTP_Message = {
  start_line: HTTP_StartLine;
  headers: HTTP_Headers*;
  body: HTTP_Body?;
}

let HTTP_StartLine = {
  RequestLine | StatusLine;
}

let RequestLine = {
  method: HTTP_Method;
  request_uri: HTTP_URI;
  http_version: HTTP_Version;
}

let StatusLine = {
  http_version: HTTP_Version;
  status_code: HTTP_StatusCode;
  reason_phrase: HTTP_ReasonPhrase?;
}

let HTTP_Method = {
  "GET" | "POST" | "PUT" | "DELETE" | "HEAD" |
  "OPTIONS" | "TRACE" | "CONNECT" | custom_method: Token;
}

let HTTP_URI = {
  scheme: "http" | "https";
  host: Host;
  port: Port?;
  path: Path?;
  query: Query?;
  fragment: Fragment?;
}

let HTTP_Version = {
  "HTTP/" digit "." digit;
}

let HTTP_StatusCode = {
  digit digit digit;
}

let HTTP_ReasonPhrase = {
  (char - ["\r", "\n"])*;
}

let HTTP_Headers = {
  Accept | Accept_Charset | Accept_Encoding | Accept_Language |
  Authorization | Cache_Control | Connection | Content_Length |
  Content_Type | Date | Expect | From | Host | If_Match |
  If_Modified_Since | If_None_Match | If_Range | If_Unmodified_Since |
  Max_Forwards | Pragma | Proxy_Authorization | Range | Referer |
  TE | User_Agent | Accept_Ranges | Age | ETag | Location |
  Proxy_Authenticate | Retry_After | Server | Vary | WWW_Authenticate |
  Allow | Content_Encoding | Content_Language | Content_Location |
  Content_MD5 | Content_Range | Expires | Last_Modified |
  Trailer | Transfer_Encoding | Upgrade | Warning | Custom_Header;
}

let Accept = {
  "Accept:" media_type ("," media_type)*;
}

let Accept_Charset = {
  "Accept-Charset:" charset ("," charset)*;
}

let Accept_Encoding = {
  "Accept-Encoding:" encoding ("," encoding)*;
}

let Accept_Language = {
  "Accept-Language:" language_tag ("," language_tag)*;
}

let Authorization = {
  "Authorization:" credentials;
}

let Cache_Control = {
  "Cache-Control:" cache_directive ("," cache_directive)*;
}

let Connection = {
  "Connection:" connection_token ("," connection_token)*;
}

let Content_Length = {
  "Content-Length:" digit+;
}

let Content_Type = {
  "Content-Type:" media_type;
}

let Date = {
  "Date:" HTTP_Date;
}

let Expect = {
  "Expect:" expectation;
}

let From = {
  "From:" email_address;
}

let Host = {
  "Host:" host (":" Port)?;
}

let If_Match = {
  "If-Match:" entity_tag ("," entity_tag | "*");
}

let If_Modified_Since = {
  "If-Modified-Since:" HTTP_Date;
}

let If_None_Match = {
  "If-None-Match:" entity_tag ("," entity_tag | "*")?;
}

let If_Range = {
  "If-Range:" entity_tag | HTTP_Date;
}

let If_Unmodified_Since = {
  "If-Unmodified-Since:" HTTP_Date;
}

let Max_Forwards = {
  "Max-Forwards:" digit+;
}

let Pragma = {
  "Pragma:" pragma_directive ("," pragma_directive)*;
}

let Proxy_Authorization = {
  "Proxy-Authorization:" credentials;
}

let Range = {
  "Range:" ranges_s极ifier;
}

let Referer = {
  "Referer:" absoluteURI | partialURI;
}

let TE = {
  "TE:" transfer_coding ("," transfer_coding)*;
}

let User_Agent = {
  "User-Agent:" product (product | comment)*;
}

let Accept_Ranges = {
  "Accept-Ranges:" range_unit | "none";
}

let Age = {
  "Age:" delta_seconds;
}

let ETag = {
  "ETag:" entity_tag;
}

let Location = {
  "Location:" absoluteURI | relativeURI;
}

let Proxy_Authenticate = {
  "Proxy-Authenticate:" challenge;
}

let Retry_After = {
  "Retry-After:" HTTP_Date | delta_seconds;
}

let Server = {
  "Server:" product (product | comment)*;
}

let Vary = {
  "Vary:" "*" | field_name ("," field_name)*;
}

let WWW_Authenticate = {
  "WWW-Authenticate:" challenge;
}

let Allow = {
  "Allow:" method ("," method)*;
}

let Content_Encoding = {
  "Content-Encoding:" content_coding ("," content_coding)*;
}

let Content_Language = {
  "Content-Language:" language_tag ("," language_tag)*;
}

let Content_Location = {
  "Content-Location:" absoluteURI | relativeURI;
}

let Content_MD5 = {
  "Content-MD5:" base64;
}

let Content_Range = {
  "Content-Range:" content_range_spec;
}

let Expires = {
  "Expires:" HTTP_Date;
}

let Last_Modified = {
  "Last-Modified:" HTTP_Date;
}

let Trailer = {
  "Trailer:" field_name ("," field_name)*;
}

let Transfer_Encoding = {
  "Transfer-Encoding:" transfer_coding ("," transfer_coding)*;
}

let Upgrade = {
  "Upgrade:" protocol ("," protocol)*;
}

let Warning = {
  "Warning:" warning_value ("," warning_value)*;
}

let Custom_Header = {
  field_name ":" field_value;
}

let HTTP_Body = {
  (octet)*;
}