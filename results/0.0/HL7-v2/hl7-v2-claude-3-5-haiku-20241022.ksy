meta:
  id: hl7_v2
  endian: le
  encoding: utf-8

types:
  message:
    seq:
      - id: segments
        type: segment
        repeat: eos

  segment:
    seq:
      - id: name
        type: str
        size: 3
      - id: fields
        type: field
        repeat: eos

  field:
    seq:
      - id: components
        type: component
        repeat: eos

  component:
    seq:
      - id: subcomponents
        type: subcomponent
        repeat: eos

  subcomponent:
    seq:
      - id: value
        type: str
        size-eos: true

seq:
  - id: message
    type: message