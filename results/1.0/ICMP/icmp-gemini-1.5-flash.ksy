type: struct
endian: big
fields:
  - id: type
    type: u1
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: identifier
    type: u2
  - id: sequence_number
    type: u2
  - id: data
    type: bytes
    size: lambda: self._io.size() - self._io.pos()
