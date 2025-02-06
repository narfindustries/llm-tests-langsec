meta:
  id: hl7_v2
  title: HL7 Version 2.x Health Care Message
  file-extension: hl7
  encoding: ascii
  endian: be
seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: segment_type
        type: str
        size: 3
      - id: fields
        type: field
        repeat: until
        repeat-until: _io.pos() == _io.size

  field:
    seq:
      - id: field
        type: str
        terminator: 0x7C
        consume: true
        include: false
        eos-error: false
    instances:
      components:
        value: field.split('^')
        type: components
        if: field != ""

  components:
    seq:
      - id: component
        type: str
        terminator: 0x5E
        consume: true
        include: false
        eos-error: false
        repeat: eos
    instances:
      subcomponents:
        value: component.split('&')
        type: subcomponents
        if: component != ""

  subcomponents:
    seq:
      - id: subcomponent
        type: str
        terminator: 0x26
        consume: true
        include: false
        eos-error: false
        repeat: eos