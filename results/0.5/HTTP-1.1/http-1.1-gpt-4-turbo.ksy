meta:
  id: http_1_1
  title: HTTP 1.1 Protocol
  application: Web Browsers, Web Servers
  xref:
    rfc: 2616
doc: |
  HTTP (Hypertext Transfer Protocol) 1.1 is a protocol used primarily for transferring data over the web. It includes methods like GET, POST, and HEAD.
seq:
  - id: request
    type: request_line
    doc: The request line contains the method, path and the HTTP version.
  - id: headers
    type: header
    repeat: eos
    doc: HTTP headers, repeated until the sequence of characters \r\n\r\n
  - id: body
    size-eos: true
    doc: Optional body, presence depends on method and headers.

types:
  request_line:
    seq:
      - id: method
        type: strz
        encoding: ASCII
        terminator: 0x20
        doc: HTTP method (GET, POST, etc.)
      - id: path
        type: strz
        encoding: ASCII
        terminator: 0x20
        doc: Path requested by the client.
      - id: http_version
        type: strz
        encoding: ASCII
        terminator: 0x0D0A # CRLF
        doc: HTTP version, typically "HTTP/1.1".

  header:
    seq:
      - id: name
        type: strz
        encoding: ASCII
        terminator: 0x3A # Colon (:)
        doc: Header name.
      - id: value
        type: strz
        encoding: ASCII
        terminator: 0x0D0A # CRLF
        doc: Header value.

  strz:
    seq:
      - id: str
        type: str
        encoding: ASCII
        terminator: 0x00
        consume: true
        include: false
        eos-error: false
        doc: Null-terminated ASCII string.