type: struct
id: icmp_header
endian: be
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
    size: lambda: self.body_size
