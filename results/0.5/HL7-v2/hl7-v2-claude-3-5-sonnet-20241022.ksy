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
      - id: field_separator
        type: u1
      - id: fields
        type: field
        repeat: until
        repeat-until: _io.eof or _ == "\r"
  field:
    seq:
      - id: content
        type: str
        terminator: 0x7c # | character
        encoding: ASCII
      - id: components
        type: component
        repeat: until
        repeat-until: _io.eof or _ == "\r"
  component:
    seq:
      - id: value
        type: str
        terminator: 0x5e # ^ character
        encoding: ASCII
      - id: subcomponents
        type: subcomponent
        repeat: until
        repeat-until: _io.eof or _ == "\r"
  subcomponent:
    seq:
      - id: value
        type: str
        terminator: 0x26 # & character
        encoding: ASCII