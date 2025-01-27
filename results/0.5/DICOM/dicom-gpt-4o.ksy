meta:
  id: dicom
  title: DICOM
  file-extension: dcm
  endian: le

seq:
  - id: preamble
    size: 128
  - id: magic
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
        type: u2
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
    value: preamble[128] == 0x44
