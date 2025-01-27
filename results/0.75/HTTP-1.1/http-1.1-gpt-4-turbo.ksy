meta:
  id: http
  title: HTTP 1.1 Protocol
  application: Web Browsers, Web Servers
  endian: be
doc: |
  HTTP/1.1 is a version of the Hypertext Transfer Protocol used by the World Wide Web.
seq:
  - id: request
    type: request_line
    doc: HTTP request line.
  - id: response
    type: response_line
    doc: HTTP response line.
  - id: headers
    type: headers
    repeat: eos
    doc: HTTP headers.
types:
  request_line:
    seq:
      - id: method
        type: strz
        encoding: ASCII
        doc: HTTP method.
      - id: uri
        type: strz
        encoding: ASCII
        doc: Request URI.
      - id: version
        type: strz
        encoding: ASCII
        doc: HTTP version.
  response_line:
    seq:
      - id: version
        type: strz
        encoding: ASCII
        doc: HTTP version.
      - id: status_code
        type: u1
        doc: HTTP status code.
      - id: reason_phrase
        type: strz
        encoding: ASCII
        doc: Reason phrase describing the status.
  headers:
    seq:
      - id: name
        type: strz
        encoding: ASCII
        doc: Header name.
      - id: value
        type: strz
        encoding: ASCII
        doc: Header value.
  strz:
    seq:
      - id: str
        type: str
        terminator: 0x0D0A
        encoding: ASCII
        doc: Null-terminated ASCII string.
