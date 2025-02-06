def HTTP = [HTTP_message]

def HTTP_message = {
  start_line : Start_Line
  headers    : Header*
  crlf       : "\n"
  body       : Body?
}

def Start_Line = Request_Line | Status_Line

def Request_Line = {
  method  : Method
  sp      : " "
  request_uri : URI
  sp      : " "
  version : HTTP_Version
  crlf    : "\n"
}

def Status_Line = {
  version : HTTP_Version
  sp      : " "
  status  : Status_Code
  sp      : " "
  reason  : Reason_Phrase
  crlf    : "\n"
}

def Method = "OPTIONS" | "GET" | "HEAD" | "POST" | "PUT" | "DELETE" | "TRACE" | "CONNECT"

def HTTP_Version = "HTTP/1.1"

def Status_Code = /[1-5][0-9][0-9]/

def Reason_Phrase = /[^\n]*/

def URI = /[^ \n]*/

def Header = {
  name  : Header_Name
  colon : ":"
  sp    : " "
  value : Header_Value
  crlf  : "\n"
}

def Header_Name = General_Header | Request_Header | Response_Header | Entity_Header

def General_Header = 
    "Cache-Control"
  | "Connection"
  | "Date"
  | "Pragma"
  | "Trailer"
  | "Transfer-Encoding"
  | "Upgrade"
  | "Via"
  | "Warning"

def Request_Header =
    "Accept"
  | "Accept-Charset"
  | "Accept-Encoding"
  | "Accept-Language"
  | "Authorization"
  | "Expect"
  | "From"
  | "Host"
  | "If-Match"
  | "If-Modified-Since"
  | "If-None-Match"
  | "If-Range"
  | "If-Unmodified-Since"
  | "Max-Forwards"
  | "Proxy-Authorization"
  | "Range"
  | "Referer"
  | "TE"
  | "User-Agent"

def Response_Header =
    "Accept-Ranges"
  | "Age"
  | "ETag"
  | "Location"
  | "Proxy-Authenticate"
  | "Retry-After"
  | "Server"
  | "Vary"
  | "WWW-Authenticate"

def Entity_Header =
    "Allow"
  | "Content-Encoding"
  | "Content-Language"
  | "Content-Length"
  | "Content-Location"
  | "Content-MD5"
  | "Content-Range"
  | "Content-Type"
  | "Expires"
  | "Last-Modified"

def Header_Value = /[^\n]*/

def Body = Chunked_Body | Regular_Body

def Chunked_Body = {
  chunks : Chunk*
  last_chunk : Last_Chunk
  trailer : Header*
  crlf : "\n"
}

def Chunk = {
  size : Chunk_Size
  crlf : "\n"
  data : /./[Int(size, 16)]
  crlf : "\n"
}

def Chunk_Size = /[0-9A-Fa-f]+/

def Last_Chunk = {
  zero  : "0"
  crlf : "\n"
}

def Regular_Body = /.*$/

def Media_Type = {
  type    : /[^\/]+/
  slash   : "/"
  subtype : /[^;\s]+/
  params  : Media_Param*
}

def Media_Param = {
  semi    : ";"
  sp      : " "?
  name    : /[^=]+/
  equals  : "="
  value   : Quoted_String | Token
}

def Quoted_String = {
  quote1   : "\""
  content  : /[^"\\]*/
  quote2   : "\""
}

def Token = /[^\/;=\s]+/

def HTTP_Date = {
  wkday    : Weekday
  comma    : ","
  sp1      : " "
  date     : Date
  sp2      : " "
  time     : Time
  sp3      : " "
  gmt      : "GMT"
}

def Weekday = "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun"

def Date = {
  day   : /[0-9]{2}/
  sp    : " "
  month : Month
  sp2   : " "
  year  : /[0-9]{4}/
}

def Month = "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"

def Time = {
  hour   : /[0-9]{2}/
  colon1 : ":"
  minute : /[0-9]{2}/
  colon2 : ":"
  second : /[0-9]{2}/
}

def Quality_Value = {
  semi    : ";"
  sp      : " "?
  q       : "q"
  equals  : "="
  value   : /[01](\.[0-9]{0,3})?/
}

def Language_Tag = {
  primary : /[a-zA-Z]{1,8}/
  subtags : Language_Subtag*
}

def Language_Subtag = {
  hyphen  : "-"
  subtag  : /[a-zA-Z0-9]{1,8}/
}

def Entity_Tag = {
  weak : "W/"?
  tag  : Quoted_String
}

def Range_Spec = {
  bytes   : "bytes"
  equals  : "="
  ranges  : Byte_Range_Spec+
}

def Byte_Range_Spec = {
  first    : /[0-9]+/?
  hyphen   : "-"
  last     : /[0-9]+/?
}

def Product_Token = {
  product : Token
  version : Product_Version?
}

def Product_Version = {
  slash   : "/"
  version : Token
}