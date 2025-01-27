meta:
  id: hl7_v2
  file-extension: hl7
  endian: le
  encoding: utf-8

seq:
  - id: segments
    type: segment
    repeat: eos

types:
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
      - id: delimiter
        type: str
        size: 1
      - id: value
        type: str
        terminator: 0x0D  # Carriage return

instances:
  message_type:
    value: segments[0].name
    
  message_version:
    value: segments[1].name if segments.size > 1 else null