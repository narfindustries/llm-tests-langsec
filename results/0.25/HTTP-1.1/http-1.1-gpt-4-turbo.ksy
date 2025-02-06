meta:
  id: http
  title: HTTP 1.1 Protocol
  application: Web Browsers, Web Servers
  xref:
    rfc: 2616
doc: |
  HTTP/1.1 protocol as defined by RFC 2616, handling both request and response structures.
seq:
  - id: request
    type: http_request
  - id: response
    type: http_response
types:
  http_request:
    seq:
      - id: method
        type: str
        terminator: 0x20
        encoding: ASCII
        doc: HTTP method, e.g., GET, POST, PUT, DELETE, etc.
      - id: uri
        type: str
        terminator: 0x20
        encoding: ASCII
        doc: Request URI.
      - id: version
        type: str
        terminator: 0x0D0A
        encoding: ASCII
        doc: HTTP version, typically "HTTP/1.1".
      - id: headers
        type: header
        repeat: eos
  http_response:
    seq:
      - id: version
        type: str
        terminator: 0x20
        encoding: ASCII
        doc: HTTP version, typically "HTTP/1.1".
      - id: status_code
        type: str
        terminator: 0x20
        encoding: ASCII
        doc: Status code returned by the server.
      - id: reason_phrase
        type: str
        terminator: 0x0D0A
        encoding: ASCII
        doc: Description of the status code.
      - id: headers
        type: header
        repeat: eos
  header:
    seq:
      - id: name
        type: str
        terminator: 0x3A  # Colon (':')
        encoding: ASCII
        doc: Name of the header.
      - id: value
        type: str
        terminator: 0x0D0A
        encoding: ASCII
        doc: Value of the header.
  str:
    seq:
      - id: value
        type: strz
        encoding: ASCII
    instances:
      stripped:
        value: |
          value.rstrip("\r\n").lstrip()
        doc: Stripped string, removing leading and trailing whitespace.
  strz:
    seq:
      - id: text
        type: str
        encoding: ASCII
        terminator: 0x0D0A
        include: false
        eos-error: false
        doc: Null-terminated ASCII string.