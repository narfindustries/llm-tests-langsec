HTTP_1_1 = {
  GeneralHeaders: {
    CacheControl: optional string,
    Connection: optional string,
    Date: optional string,
    Pragma: optional string,
    Trailer: optional string,
    TransferEncoding: optional string,
    Upgrade: optional string,
    Via: optional string,
    Warning: optional string
  },
  RequestHeaders: {
    Accept: optional string,
    AcceptCharset: optional string,
    AcceptEncoding: optional string,
    AcceptLanguage: optional string,
    Authorization: optional string,
    Expect: optional string,
    From: optional string,
    Host: string,
    IfMatch: optional string,
    IfModifiedSince: optional string,
    IfNoneMatch: optional string,
    IfRange: optional string,
    IfUnmodifiedSince: optional string,
    MaxForwards: optional string,
    ProxyAuthorization: optional string,
    Range: optional string,
    Referer: optional string,
    TE: optional string,
    UserAgent: optional string
  },
  ResponseHeaders: {
    AcceptRanges: optional string,
    Age: optional string,
    ETag: optional string,
    Location: optional string,
    ProxyAuthenticate: optional string,
    RetryAfter: optional string,
    Server: optional string,
    Vary: optional string,
    WWWAuthenticate: optional string
  },
  EntityHeaders: {
    Allow: optional string,
    ContentEncoding: optional string,
    ContentLanguage: optional string,
    ContentLength: optional string,
    ContentLocation: optional string,
    ContentMD5: optional string,
    ContentRange: optional string,
    ContentType: optional string,
    Expires: optional string,
    LastModified: optional string
  },
  RequestLine: {
    Method: string,
    RequestURI: string,
    HTTPVersion: string
  },
  StatusLine: {
    HTTPVersion: string,
    StatusCode: string,
    ReasonPhrase: string
  },
  MessageBody: optional string
}