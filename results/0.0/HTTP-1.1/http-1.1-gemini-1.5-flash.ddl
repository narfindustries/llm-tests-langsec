type HttpHeader = {
  name: String,
  value: String
}

type HttpStartLine = {
  version: String,
  statusCode: Maybe Int,
  reasonPhrase: Maybe String,
  method: Maybe String,
  requestTarget: Maybe String
}

type HttpMessage = {
  startLine: HttpStartLine,
  headers: Seq[HttpHeader],
  body: Bytes
}

type HttpRequest = {
  startLine: { version: String, method: String, requestTarget: String },
  headers: Seq[HttpHeader],
  body: Bytes
}

type HttpResponse = {
  startLine: { version: String, statusCode: Int, reasonPhrase: String },
  headers: Seq[HttpHeader],
  body: Bytes
}
