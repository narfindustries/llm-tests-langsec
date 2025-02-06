def Version:
  major: u8
  minor: u8

def Method:
  | GET
  | POST
  | PUT
  | DELETE
  | HEAD
  | OPTIONS
  | TRACE
  | CONNECT
  | PATCH

def Header:
  name: string
  value: string

def Headers: list(Header)

def Request:
  method: Method
  uri: string
  version: Version
  headers: Headers
  body: optional(bytes)

def Response:
  version: Version
  status_code: u16
  status_text: string
  headers: Headers
  body: optional(bytes)

def CacheControl:
  | NoCache
  | NoStore
  | MaxAge(u32)
  | Private
  | Public
  | MustRevalidate

def ConnectionType:
  | Close
  | KeepAlive

def AcceptEncoding:
  | Gzip
  | Deflate
  | Compress
  | Identity

def ContentType:
  | TextHtml
  | ApplicationJson
  | ApplicationXml
  | MultipartFormData

def Authorization:
  | Basic(string)
  | Digest(string)
  | Bearer(string)

def Message:
  | RequestMsg(Request)
  | ResponseMsg(Response)

parser parse_version:
  major: u8
  minor: u8

parser parse_method:
  | "GET" => Method.GET
  | "POST" => Method.POST
  | "PUT" => Method.PUT
  | "DELETE" => Method.DELETE
  | "HEAD" => Method.HEAD
  | "OPTIONS" => Method.OPTIONS
  | "TRACE" => Method.TRACE
  | "CONNECT" => Method.CONNECT
  | "PATCH" => Method.PATCH

parser parse_header:
  name: string
  value: string

parser parse_request:
  method: parse_method
  uri: string
  version: parse_version
  headers: list(parse_header)
  body: optional(bytes)

parser parse_response:
  version: parse_version
  status_code: u16
  status_text: string
  headers: list(parse_header)
  body: optional(bytes)