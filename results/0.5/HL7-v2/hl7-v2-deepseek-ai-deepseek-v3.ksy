meta:
  id: hl7_v2
  title: HL7 Version 2.x
  license: CC0-1.0
  endian: be
seq:
  - id: segments
    type: segment
    repeat: eos
types:
  segment:
    seq:
      - id: segment_id
        type: str
        encoding: ASCII
        size: 3
      - id: fields
        type: field
        repeat: until
        repeat-until: _io.eof
  field:
    seq:
      - id: field_value
        type: str
        encoding: ASCII
        size-eos: true
        terminator: 124
      - id: components
        type: component
        repeat: until
        repeat-until: _io.eof
  component:
    seq:
      - id: component_value
        type: str
        encoding: ASCII
        size-eos: true
        terminator: 94
      - id: subcomponents
        type: subcomponent
        repeat: until
        repeat-until: _io.eof
  subcomponent:
    seq:
      - id: subcomponent_value
        type: str
        encoding: ASCII
        size-eos: true
        terminator: 38