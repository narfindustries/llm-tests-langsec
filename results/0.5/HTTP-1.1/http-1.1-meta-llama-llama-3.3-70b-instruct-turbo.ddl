domain http_1_1 {
  type request = {
    method: string(3, upper),
    space: byte = 0x20,
    path: string(1, 4096, uri),
    question_mark: byte = 0x3f,
    query: string(0, 4096, query),
    http_version: string = "HTTP/1.1",
    crlf: bytes = [0x0d, 0x0a],
    headers: while not-done {
      header_name: string(1, 64, token),
      colon: byte = 0x3a,
      space: byte = 0x20,
      header_value: string(0, 4096, text),
      crlf: bytes = [0x0d, 0x0a],
      if header_name != "Content-Length" and
         header_name != "Transfer-Encoding"
    },
    body: if headers has "Content-Length" {
      length: int32 be,
      data: bytes(length)
    } else if headers has "Transfer-Encoding" {
      chunks: while not-done {
        length: hex(1, 8),
        crlf: bytes = [0x0d, 0x0a],
        chunk: bytes(length),
        crlf: bytes = [0x0d, 0x0a]
      },
      final_crlf: bytes = [0x0d, 0x0a]
    }
  };

  type response = {
    http_version: string = "HTTP/1.1",
    space: byte = 0x20,
    status_code: string(3, digit),
    space: byte = 0x20,
    reason_phrase: string(1, 64, text),
    crlf: bytes = [0x0d, 0x0a],
    headers: while not-done {
      header_name: string(1, 64, token),
      colon: byte = 0x3a,
      space: byte = 0x20,
      header_value: string(0, 4096, text),
      crlf: bytes = [0x0d, 0x0a],
      if header_name != "Content-Length" and
         header_name != "Transfer-Encoding"
    },
    body: if headers has "Content-Length" {
      length: int32 be,
      data: bytes(length)
    } else if headers has "Transfer-Encoding" {
      chunks: while not-done {
        length: hex(1, 8),
        crlf: bytes = [0x0d, 0x0a],
        chunk: bytes(length),
        crlf: bytes = [0x0d, 0x0a]
      },
      final_crlf: bytes = [0x0d, 0x0a]
    }
  };
}