format http {
  type HTTP_Message {
    choice {
      Request: HTTP_Request,
      Response: HTTP_Response
    }
  }

  type HTTP_Request {
    struct {
      method: choice {
        GET: string = "GET",
        HEAD: string = "HEAD",
        POST: string = "POST",
        PUT: string = "PUT",
        DELETE: string = "DELETE",
        CONNECT: string = "CONNECT",
        OPTIONS: string = "OPTIONS",
        TRACE: string = "TRACE"
      },
      request_uri: choice {
        absoluteURI: string,
        abs_path: string,
        authority: string
      },
      http_version: string = "HTTP/1.1",
      headers: array[HTTP_Header],
      body: optional[byte_array]
    }
  }

  type HTTP_Response {
    struct {
      http_version: string = "HTTP/1.1",
      status_code: choice {
        Informational: choice {
          Continue: uint16 = 100,
          SwitchingProtocols: uint16 = 101
        },
        Successful: choice {
          OK: uint16 = 200,
          Created: uint16 = 201,
          Accepted: uint16 = 202,
          NonAuthoritativeInformation: uint16 = 203,
          NoContent: uint16 = 204,
          ResetContent: uint16 = 205,
          PartialContent: uint16 = 206
        },
        Redirection: choice {
          MultipleChoices: uint16 = 300,
          MovedPermanently: uint16 = 301,
          Found: uint16 = 302,
          SeeOther: uint16 = 303,
          NotModified: uint16 = 304,
          UseProxy: uint16 = 305,
          TemporaryRedirect: uint16 = 307
        },
        ClientError: choice {
          BadRequest: uint16 = 400,
          Unauthorized: uint16 = 401,
          PaymentRequired: uint16 = 402,
          Forbidden: uint16 = 403,
          NotFound: uint16 = 404,
          MethodNotAllowed: uint16 = 405,
          NotAcceptable: uint16 = 406,
          ProxyAuthenticationRequired: uint16 = 407,
          RequestTimeOut: uint16 = 408,
          Conflict: uint16 = 409,
          Gone: uint16 = 410,
          LengthRequired: uint16 = 411,
          PreconditionFailed: uint16 = 412,
          RequestEntityTooLarge: uint16 = 413,
          RequestURITooLarge: uint16 = 414,
          UnsupportedMediaType: uint16 = 415,
          RequestedRangeNotSatisfiable: uint16 = 416,
          ExpectationFailed: uint16 = 417
        },
        ServerError: choice {
          InternalServerError: uint16 = 500,
          NotImplemented: uint16 = 501,
          BadGateway: uint16 = 502,
          ServiceUnavailable: uint16 = 503,
          GatewayTimeOut: uint16 = 504,
          HTTPVersionNotSupported: uint16 = 505
        }
      },
      reason_phrase: string,
      headers: array[HTTP_Header],
      body: optional[byte_array]
    }
  }

  type HTTP_Header {
    struct {
      header_name: string,
      header_value: string
    }
  }

  type HTTP_Header_Name {
    choice {
      Accept: string = "Accept",
      AcceptCharset: string = "Accept-Charset",
      AcceptEncoding: string = "Accept-Encoding",
      AcceptLanguage: string = "Accept-Language",
      Authorization: string = "Authorization",
      CacheControl: string = "Cache-Control",
      Connection: string = "Connection",
      ContentEncoding: string = "Content-Encoding",
      ContentLanguage: string = "Content-Language",
      ContentLength: string = "Content-Length",
      ContentLocation: string = "Content-Location",
      ContentMD5: string = "Content-MD5",
      ContentRange: string = "Content-Range",
      ContentType: string = "Content-Type",
      Date: string = "Date",
      Expect: string = "Expect",
      From: string = "From",
      Host: string = "Host",
      IfMatch: string = "If-Match",
      IfModifiedSince: string = "If-Modified-Since",
      IfNoneMatch: string = "If-None-Match",
      IfRange: string = "If-Range",
      IfUnmodifiedSince: string = "If-Unmodified-Since",
      MaxForwards: string = "Max-Forwards",
      ProxyAuthorization: string = "Proxy-Authorization",
      Range: string = "Range",
      Referer: string = "Referer",
      TE: string = "TE",
      Upgrade: string = "Upgrade",
      UserAgent: string = "User-Agent",
      Via: string = "Via",
      Warning: string = "Warning",
      Allow: string = "Allow",
      ContentBase: string = "Content-Base",
      Expires: string = "Expires",
      LastModified: string = "Last-Modified",
      Location: string = "Location",
      ProxyAuthenticate: string = "Proxy-Authenticate",
      RetryAfter: string = "Retry-After",
      Server: string = "Server",
      Vary: string = "Vary",
      WWWAuthenticate: string = "WWW-Authenticate",
      Age: string = "Age",
      ETag: string = "ETag"
    }
  }
}