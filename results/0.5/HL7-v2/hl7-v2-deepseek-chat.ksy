meta:
  id: hl7_v2_deepseek_chat
  title: HL7 Version 2 Message Format
  file-extension: hl7
  encoding: UTF-8
  endian: be
  license: MIT
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
      - id: fields
        type: field
        repeat: until
        repeat-until: _io.eof
  field:
    seq:
      - id: field_value
        type: str
        size-eos: true
        encoding: UTF-8
      - id: field_separator
        type: u1
        if: _io.pos < _io.size