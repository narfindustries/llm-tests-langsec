DEFINE STRUCT HTTP_Message {
  request_line: Request_Line,
  headers: Headers,
  body: OPTIONAL BYTE_ARRAY
}

DEFINE STRUCT Request_Line {
  method: ENUM("GET", "HEAD", "POST", "PUT", "DELETE", "CONNECT", "OPTIONS", "TRACE"),
  request_uri: STRING,
  http_version: STRING
}

DEFINE STRUCT Headers {
  header_fields: ARRAY OF Header_Field
}

DEFINE STRUCT Header_Field {
  field_name: STRING,
  field_value: STRING
}

DEFINE STRUCT Response {
  status_line: Status_Line,
  headers: Headers,
  body: OPTIONAL BYTE_ARRAY
}

DEFINE STRUCT Status_Line {
  http_version: STRING,
  status_code: INTEGER,
  reason_phrase: STRING
}

DEFINE ENUM Status_Code {
  100, 101,
  200, 201, 202, 203, 204, 205, 206,
  300, 301, 302, 303, 304, 305, 307,
  400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417,
  500, 501, 502, 503, 504, 505
}

DEFINE ENUM Header_Fields {
  "Accept",
  "Accept-Charset",
  "Accept-Encoding",
  "Accept-Language",
  "Authorization",
  "Cache-Control",
  "Connection",
  "Content-Length",
  "Content-Type",
  "Date",
  "Expect",
  "From",
  "Host",
  "If-Match",
  "If-Modified-Since",
  "If-None-Match",
  "If-Range",
  "If-Unmodified-Since",
  "Max-Forwards",
  "Proxy-Authorization",
  "Range",
  "Referer",
  "TE",
  "User-Agent",
  "Age",
  "ETag",
  "Location",
  "Proxy-Authenticate",
  "Retry-After",
  "Server",
  "Vary",
  "WWW-Authenticate",
  "Content-Location",
  "Content-MD5",
  "Content-Range",
  "Expires",
  "Last-Modified"
}

DEFINE STRUCT Accept {
  media_type: STRING,
  accept_params: OPTIONAL ARRAY OF Accept_Param
}

DEFINE STRUCT Accept_Param {
  param_name: STRING,
  param_value: STRING
}

DEFINE STRUCT Accept_Charset {
  charset: STRING
}

DEFINE STRUCT Accept_Encoding {
  content_coding: STRING
}

DEFINE STRUCT Accept_Language {
  language_tag: STRING
}

DEFINE STRUCT Authorization {
  auth_scheme: STRING,
  auth_param: OPTIONAL ARRAY OF Auth_Param
}

DEFINE STRUCT Auth_Param {
  param_name: STRING,
  param_value: STRING
}

DEFINE STRUCT Cache_Control {
  cache_directive: STRING
}

DEFINE STRUCT Connection {
  connection_token: STRING
}

DEFINE STRUCT Content_Length {
  length: INTEGER
}

DEFINE STRUCT Content_Type {
  media_type: STRING,
  content_type_params: OPTIONAL ARRAY OF Content_Type_Param
}

DEFINE STRUCT Content_Type_Param {
  param_name: STRING,
  param_value: STRING
}

DEFINE STRUCT Date {
  http_date: STRING
}

DEFINE STRUCT Expect {
  expect_param: STRING
}

DEFINE STRUCT From {
  mailbox: STRING
}

DEFINE STRUCT Host {
  host: STRING,
  port: OPTIONAL INTEGER
}

DEFINE STRUCT If_Match {
  entity_tag: STRING
}

DEFINE STRUCT If_Modified_Since {
  http_date: STRING
}

DEFINE STRUCT If_None_Match {
  entity_tag: STRING
}

DEFINE STRUCT If_Range {
  entity_tag: STRING,
  http_date: OPTIONAL STRING
}

DEFINE STRUCT If_Unmodified_Since {
  http_date: STRING
}

DEFINE STRUCT Max_Forwards {
  max_forwards: INTEGER
}

DEFINE STRUCT Proxy_Authorization {
  auth_scheme: STRING,
  auth_param: OPTIONAL ARRAY OF Auth_Param
}

DEFINE STRUCT Range {
  byte_range_spec: STRING
}

DEFINE STRUCT Referer {
  absolute_uri: STRING
}

DEFINE STRUCT TE {
  transfer_extension: STRING
}

DEFINE STRUCT User_Agent {
  product: STRING,
  comment: OPTIONAL STRING
}

DEFINE STRUCT Age {
  age: INTEGER
}

DEFINE STRUCT ETag {
  entity_tag: STRING
}

DEFINE STRUCT Location {
  absolute_uri: STRING
}

DEFINE STRUCT Proxy_Authenticate {
  auth_scheme: STRING,
  auth_param: OPTIONAL ARRAY OF Auth_Param
}

DEFINE STRUCT Retry_After {
  http_date: STRING,
  delay_seconds: OPTIONAL INTEGER
}

DEFINE STRUCT Server {
  product: STRING,
  comment: OPTIONAL STRING
}

DEFINE STRUCT Vary {
  field_name: STRING
}

DEFINE STRUCT WWW_Authenticate {
  auth_scheme: STRING,
  auth_param: OPTIONAL ARRAY OF Auth_Param
}

DEFINE STRUCT Content_Location {
  absolute_uri: STRING
}

DEFINE STRUCT Content_MD5 {
  md5_digest: STRING
}

DEFINE STRUCT Content_Range {
  byte_range_spec: STRING
}

DEFINE STRUCT Expires {
  http_date: STRING
}

DEFINE STRUCT Last_Modified {
  http_date: STRING
}