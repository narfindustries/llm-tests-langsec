meta:
  id: hl7_v2
  title: Health Level 7 (HL7) Version 2
  application: Health Information Systems
  file-extension: hl7
  xref:
    wikidata: Q1475216
doc: |
  HL7 Version 2.x messages are used in clinical information systems to send and receive patient information. They are composed of segments, each containing fields, components, and sub-components.

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
        encoding: ASCII
      - id: fields
        type: field
        repeat: until
        repeat-until: _.is_field_separator
        repeat-until-eos: true

  field:
    seq:
      - id: components
        type: component
        repeat: until
        repeat-until: _.is_component_separator
        repeat-until-eos: true

  component:
    seq:
      - id: sub_components
        type: sub_component
        repeat: until
        repeat-until: _.is_sub_component_separator
        repeat-until-eos: true

  sub_component:
    seq:
      - id: value
        type: str
        encoding: ASCII
        terminator: 0x26
        include: false
        eos-error: false

instances:
  is_field_separator:
    value: "(_io.pos + 1 < _io.size and (_io.read_bytes(1) == b'\r' or _io.read_bytes(1) == b'\n' or _io.read_bytes(1) == b'|'))"
  is_component_separator:
    value: "(_io.pos + 1 < _io.size and _io.read_bytes(1) == b'^')"
  is_sub_component_separator:
    value: "(_io.pos + 1 < _io.size and _io.read_bytes(1) == b'&')"