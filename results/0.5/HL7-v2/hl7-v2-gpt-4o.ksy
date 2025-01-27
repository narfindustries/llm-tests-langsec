meta:
  id: hl7_v2
  title: HL7 Version 2.x Message
  file-extension: hl7
  endian: be

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: fields
        type: field
        repeat: until
        repeat-until: _io.eof || _io.peek_byte() == 0x0d

  field:
    seq:
      - id: components
        type: component
        repeat: until
        repeat-until: _io.eof || _io.peek_byte() == 0x7c || _io.peek_byte() == 0x0d

  component:
    seq:
      - id: subcomponents
        type: subcomponent
        repeat: until
        repeat-until: _io.eof || _io.peek_byte() == 0x5e || _io.peek_byte() == 0x7c || _io.peek_byte() == 0x0d

  subcomponent:
    seq:
      - id: value
        type: strz
        encoding: utf-8
        terminator: [0x26, 0x5e, 0x7c, 0x0d]