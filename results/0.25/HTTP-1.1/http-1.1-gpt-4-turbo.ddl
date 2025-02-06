type HttpVersion = "HTTP/" + Digit + "." + Digit;

type Request = struct {
  method: Token;
  uri: RequestURI;
  version: HttpVersion;
  headers: Headers;
  body: Octet*;
};

type Response = struct {
  version: HttpVersion;
  status: StatusCode;
  reason: ReasonPhrase;
  headers: Headers;
  body: Octet*;
};

type Headers = list(Header);

type Header = struct {
  name: Token;
  value: FieldValue;
};

type Token = regex("[!#$%&'*+-.^_`|~0-9A-Za-z]+");
type FieldValue = regex("[^\r\n]*");
type Octet = byte;

type StatusCode = uint16;
type ReasonPhrase = regex("[^\r\n]*");

type RequestURI = regex("[^\x00-\x20\x7F]*"); // Simplified URI

// Common Header Fields
type GeneralHeader = struct {
  cacheControl: optional("Cache-Control" ":" regex("[^\r\n]*"));
  connection: optional("Connection" ":" regex("[^\r\n]*"));
  date: optional("Date" ":" regex("[^\r\n]*"));
  pragma: optional("Pragma" ":" regex("[^\r\n]*"));
  trailer: optional("Trailer" ":" regex("[^\r\n]*"));
  transferEncoding: optional("Transfer-Encoding" ":" regex("[^\r\n]*"));
  upgrade: optional("Upgrade" ":" regex("[^\r\n]*"));
  via: optional("Via" ":" regex("[^\r\n]*"));
  warning: optional("Warning" ":" regex("[^\r\n]*"));
};

// Request Header Fields
type RequestHeader = struct {
  accept: optional("Accept" ":" regex("[^\r\n]*"));
  acceptCharset: optional("Accept-Charset" ":" regex("[^\r\n]*"));
  acceptEncoding: optional("Accept-Encoding" ":" regex("[^\r\n]*"));
  acceptLanguage: optional("Accept-Language" ":" regex("[^\r\n]*"));
  authorization: optional("Authorization" ":" regex("[^\r\n]*"));
  expect: optional("Expect" ":" regex("[^\r\n]*"));
  from: optional("From" ":" regex("[^\r\n]*"));
  host: optional("Host" ":" regex("[^\r\n]*"));
  ifMatch: optional("If-Match" ":" regex("[^\r\n]*"));
  ifModifiedSince: optional("If-Modified-Since" ":" regex("[^\r\n]*"));
  ifNoneMatch: optional("If-None-Match" ":" regex("[^\r\n]*"));
  ifRange: optional("If-Range" ":" regex("[^\r\n]*"));
  ifUnmodifiedSince: optional("If-Unmodified-Since" ":" regex("[^\r\n]*"));
  maxForwards: optional("Max-Forwards" ":" regex("[^\r\n]*"));
  proxyAuthorization: optional("Proxy-Authorization" ":" regex("[^\r\n]*"));
  range: optional("Range" ":" regex("[^\r\n]*"));
  referer: optional("Referer" ":" regex("[^\r\n]*"));
  te: optional("TE" ":" regex("[^\r\n]*"));
  userAgent: optional("User-Agent" ":" regex("[^\r\n]*"));
};

// Response Header Fields
type ResponseHeader = struct {
  acceptRanges: optional("Accept-Ranges" ":" regex("[^\r\n]*"));
  age: optional("Age" ":" regex("[^\r\n]*"));
  eTag: optional("ETag" ":" regex("[^\r\n]*"));
  location: optional("Location" ":" regex("[^\r\n]*"));
  proxyAuthenticate: optional("Proxy-Authenticate" ":" regex("[^\r\n]*"));
  retryAfter: optional("Retry-After" ":" regex("[^\r\n]*"));
  server: optional("Server" ":" regex("[^\r\n]*"));
  vary: optional("Vary" ":" regex("[^\r\n]*"));
  wwwAuthenticate: optional("WWW-Authenticate" ":" regex("[^\r\n]*"));
};