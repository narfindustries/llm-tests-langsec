meta:
  id: dicom
  file-extension: dcm
  endian: le
seq:
  - id: preamble
    size: 128
  - id: prefix
    contents: "DICM"
  - id: elements
    type: element
    repeat: eos
types:
  element:
    seq:
      - id: tag
        type: tag
      - id: vr
        type: vr
      - id: length
        type: length
      - id: value
        size: length.value
  tag:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2
  vr:
    seq:
      - id: value
        type: str
        size: 2
        encoding: ASCII
  length:
    seq:
      - id: value
        type: u2