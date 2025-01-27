meta:
  id: icmp
  file-extension: icmp
  endian: big
seq:
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
    type: u1
    repeat: expr
    repeat-expr: _io.size - 8