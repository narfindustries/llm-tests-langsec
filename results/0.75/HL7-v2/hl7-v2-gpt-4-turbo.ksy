meta:
  id: hl7_v2
  title: HL7 Version 2 Message
  file-extension: hl7
  encoding: ascii
  endian: le
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
      - id: segment_body
        type: str
        terminator: 0x0D # Carriage Return, '\r'
    instances:
      fields:
        value: segment_body.split('|', limit=-1)
        type: field
        repeat: expr
        repeat-expr: len(fields)

  field:
    seq:
      - id: components
        type: component
        repeat: eos
        terminator: '^'

  component:
    seq:
      - id: subcomponents
        type: subcomponent
        repeat: eos
        terminator: '&'

  subcomponent:
    seq:
      - id: data
        type: str
        terminator: 0x7C # Pipe, '|'
        consume: true
        include: false
        eos-error: false