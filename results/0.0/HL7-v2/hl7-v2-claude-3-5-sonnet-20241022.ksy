meta:
  id: hl7_v2
  file-extension: hl7
  endian: le

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
      - id: separator
        type: u1
      - id: fields
        type: field
        repeat: until
        repeat-until: _.field_content == "\r"

  field:
    seq:
      - id: field_content
        type: str
        terminator: 0x0d
        encoding: ASCII
        eos-error: false

  component:
    seq:
      - id: component_content
        type: str
        terminator: 0x5e
        encoding: ASCII
        eos-error: false

  subcomponent:
    seq:
      - id: subcomponent_content
        type: str
        terminator: 0x26
        encoding: ASCII
        eos-error: false

enums:
  segment_types:
    0x4D5348: MSH  # Message Header
    0x505449: PID  # Patient Identification
    0x4F5243: ORC  # Order Control
    0x4F4252: OBR  # Observation Request
    0x4F4258: OBX  # Observation Result