meta:
  id: hl7v2
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
        if: segment_id != "MSH"
      - id: msh_fields
        type: msh_field
        repeat: until
        repeat-until: _io.eof
        if: segment_id == "MSH"
  field:
    seq:
      - id: components
        type: component
        repeat: until
        repeat-until: _io.eof
  msh_field:
    seq:
      - id: components
        type: msh_component
        repeat: until
        repeat-until: _io.eof
  component:
    seq:
      - id: subcomponents
        type: subcomponent
        repeat: until
        repeat-until: _io.eof
  msh_component:
    seq:
      - id: subcomponents
        type: msh_subcomponent
        repeat: until
        repeat-until: _io.eof
  subcomponent:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  msh_subcomponent:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
enums:
  segment_id:
    MSH: MSH
    PID: PID
    PV1: PV1
  field_separator:
    "|": "|"
  component_separator:
    "^": "^"
  subcomponent_separator:
    "&": "&"
  repetition_separator:
    "~": "~"
  escape_character:
    "\\": "\\"