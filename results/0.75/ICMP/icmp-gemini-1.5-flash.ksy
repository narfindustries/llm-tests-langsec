type: struct
endian: be
fields:
  - id: type
    type: u1
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: body
    type: bytes
    size: lambda (self._io.size() - 4)

