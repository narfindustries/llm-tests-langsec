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
        if: _parent.vr == "OB" or _parent.vr == "OW" or _parent.vr == "OF" or _parent.vr == "SQ" or _parent.vr == "UT" or _parent.vr == "UN"
      - id: value_length
        type: u4
        if: _parent.vr == "OB" or _parent.vr == "OW" or _parent.vr == "OF" or _parent.vr == "SQ" or _parent.vr == "UT" or _parent.vr == "UN"
      - id: value_length_short
        type: u2
        if: not (_parent.vr == "OB" or _parent.vr == "OW" or _parent.vr == "OF" or _parent.vr == "SQ" or _parent.vr == "UT" or _parent.vr == "UN")
      - id: value
        size: value_length if value_length != null else value_length_short