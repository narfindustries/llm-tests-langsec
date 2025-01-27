meta:
  id: dicom
  title: DICOM File Format
  file-extension: dcm
  license: MIT
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
        if: vr.is_explicit
      - id: value
        size: length.value
        if: length.value != 0
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
    instances:
      is_explicit:
        value: value != "UN"
  length:
    seq:
      - id: value
        type: u2
        if: _parent.vr.is_explicit