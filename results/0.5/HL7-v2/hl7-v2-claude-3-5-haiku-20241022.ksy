meta:
  id: hl7_v2
  file-extension: hl7
  endian: le
  encoding: UTF-8

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: identifier
        type: str
        size: 3
        encoding: ASCII
      - id: fields
        type: field
        repeat: eos
        
  field:
    seq:
      - id: delimiter
        type: str
        size: 1
      - id: content
        type: str
        terminator: 0x0D
        consume: false