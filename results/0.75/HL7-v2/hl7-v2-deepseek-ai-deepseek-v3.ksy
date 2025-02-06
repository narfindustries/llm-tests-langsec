meta:
  id: hl7v2
  title: HL7 Version 2.x
  license: CC0-1.0
  endian: be
  encoding: ASCII
  ks-version: 0.9

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: segment_id
        type: str
        size: 3
        encoding: ASCII
      - id: fields
        type: field
        repeat: until
        repeat-until: _io.eof
        repeat-expr: _io.peek_bytes(1) == b'\r' or _io.peek_bytes(1) == b'\n'

  field:
    seq:
      - id: components
        type: component
        repeat: until
        repeat-until: _io.eof
        repeat-expr: _io.peek_bytes(1) == b'|' or _io.peek_bytes(1) == b'^' or _io.peek_bytes(1) == b'~' or _io.peek_bytes(1) == b'&' or _io.peek_bytes(1) == b'\r' or _io.peek_bytes(1) == b'\n'

  component:
    seq:
      - id: subcomponents
        type: subcomponent
        repeat: until
        repeat-until: _io.eof
        repeat-expr: _io.peek_bytes(1) == b'^' or _io.peek_bytes(1) == b'~' or _io.peek_bytes(1) == b'&' or _io.peek_bytes(1) == b'\r' or _io.peek_bytes(1) == b'\n'

  subcomponent:
    seq:
      - id: value
        type: str
        size-eos: true
        encoding: ASCII

enums:
  segment_id:
    0x4d5348: MSH
    0x504944: PID
    0x4e4b31: NK1
    0x505631: PV1
    0x4f5231: OR1
    0x4f4231: OB1
    0x4f5258: ORX
    0x4f4258: OBX
    0x535543: SUC
    0x4d5347: MSG
    0x455243: ERC
    0x44534c: DSL
    0x444553: DES