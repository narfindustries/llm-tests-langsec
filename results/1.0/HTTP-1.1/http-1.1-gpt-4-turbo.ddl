module HTTP_1_1 {
  type byte = UInt(8)
  type uint16 = UInt(16)
  type uint32 = UInt(32)

  type ASCII = Byte

  fun crlf = "\r\n" : ASCII*;

  type Method = {"GET" | "POST" | "PUT" | "DELETE" | "HEAD" | "OPTIONS" | "PATCH" | "CONNECT" | "TRACE"}
  type Version = "HTTP/" + UDec + "." + UDec
  
  type RequestLine = {
    method: Method,
    space1: ' ',
    uri: Until("\r\n", Except("\r\n ")),
    space2: ' ',
    version: Version,
    crlf: crlf
  }

  type StatusMsg = Until("\r\n", ASCII)
  
  type StatusLine = {
    version: Version,
    space1: ' ',
    statusCode: UInt(3),
    space2: ' ',
    reasonPhrase: StatusMsg,
    crlf: crlf
  }
  
  type Header = {
    fieldName: While(":", IsUpper),
    delim: ":",
    fieldValue: Until("\r\n", IsSpace | ASCII),
    crlf: crlf
  }

  type Headers = List(Header, "\r\n")

  type Body = Data
  
  type Message = Data

  type Request = {
    requestLine: RequestLine,
    headers: Headers,
    crlf: crlf,
    messageBody: Option(Body)
  }

  type Response = {
    statusLine: StatusLine,
    headers: Headers,
    crlf: crlf,
    messageBody: Option(Body)
  }
}