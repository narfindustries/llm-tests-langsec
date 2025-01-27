HTTP_1_1 = {
  start_line: StartLine,
  headers: Headers,
  body: Body
}

StartLine = {
  method: Method,
  request_target: RequestTarget,
  http_version: HttpVersion
}

Method = /[A-Z]+/

RequestTarget = /[^\s]+/

HttpVersion = "HTTP/" /[0-9]+\.[0-9]+/

Headers = {
  header: Header*
}

Header = {
  field_name: FieldName,
  field_value: FieldValue
}

FieldName = /[^:\r\n]+/

FieldValue = /[^\r\n]+/

Body = /.*/