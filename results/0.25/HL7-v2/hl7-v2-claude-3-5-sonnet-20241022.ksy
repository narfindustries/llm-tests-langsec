meta:
  id: hl7v2
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
        repeat-until: _.field_separator == 0x0d || _.field_separator == 0x0a
    
  field:
    seq:
      - id: content
        type: str
        terminator: 0x7c  # | character
        encoding: ASCII
      - id: field_separator
        type: u1

enums:
  segment_types:
    77.83.72: MSH  # ASCII values for "MSH"
    80.73.68: PID  # ASCII values for "PID"
    80.86.49: PV1  # ASCII values for "PV1"
    79.82.67: ORC  # ASCII values for "ORC"
    79.66.82: OBR  # ASCII values for "OBR"
    79.66.88: OBX  # ASCII values for "OBX"

instances:
  is_valid:
    value: segments[0].segment_type == "MSH"