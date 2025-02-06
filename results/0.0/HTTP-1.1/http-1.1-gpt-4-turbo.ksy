meta:
  id: http
  title: HTTP 1.1 Protocol
  application: Web Browsers, Web Servers
  xref:
    rfc: 2616
doc: |
  HTTP 1.1 (Hypertext Transfer Protocol) is a stateless protocol used primarily for transferring
  web documents (HTML, images, etc.). This specification covers parsing of HTTP request and response
  messages.
seq:
  - id: request
    type: http_request
  - id: response
    type: http_response
types:
  http_request:
    seq:
      - id: method
        type: strz
        encoding: ASCII
        terminator: 0x20
        doc: HTTP method (GET, POST, etc.)
      - id: uri
        type: strz
        encoding: ASCII
        terminator: 0x20
        doc: Request URI
      - id: version
        type: strz
        encoding: ASCII
        terminator: 0x0D_0A
        doc: HTTP version
      - id: headers
        type: header
        repeat: eos
  http_response:
    seq:
      - id: version
        type: strz
        encoding: ASCII
        terminator: 0x20
        doc: HTTP version
      - id: status_code
        type: strz
        encoding: ASCII
        terminator: 0x20
        doc: Status code (e.g., 200, 404)
      - id: reason_phrase
        type: strz
        encoding: ASCII
        terminator: 0x0D_0A
        doc: Reason phrase (e.g., OK, Not Found)
      - id: headers
        type: header
        repeat: eos
  header:
    seq:
      - id: name
        type: strz
        encoding: ASCII
        terminator: 0x3A_20  # Colon followed by space
        doc: Header name
      - id: value
        type: strz
        encoding: ASCII
        terminator: 0x0D_0A
        doc: Header value
    doc: |
      Each header is a name-value pair. Names are case-insensitive.
      The value is terminated by CRLF.