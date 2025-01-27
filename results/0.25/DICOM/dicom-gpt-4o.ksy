meta:
  id: dicom
  title: DICOM
  file-extension: dcm
  endian: le
  encoding: utf-8

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
      - id: tag_group
        type: u2
      - id: tag_element
        type: u2
      - id: vr
        type: str
        size: 2
      - id: reserved
        size: 2
        if: _parent.is_explicit_vr
      - id: value_length
        type: u4
        if: _parent.is_explicit_vr
      - id: value_length_implicit
        type: u2
        if: not _parent.is_explicit_vr
      - id: value
        size: value_length
        if: _parent.is_explicit_vr
      - id: value_implicit
        size: value_length_implicit
        if: not _parent.is_explicit_vr

instances:
  is_explicit_vr:
    value: preamble[128] == 0x44 and preamble[129] == 0x49 and preamble[130] == 0x43 and preamble[131] == 0x4d