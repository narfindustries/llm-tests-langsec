meta:
  id: dicom
  title: DICOM
  file-extension: dcm
  endian: le
  application: Digital Imaging and Communications in Medicine
  license: CC0-1.0
  ks-version: 0.9

seq:
  - id: preamble
    size: 128
  - id: prefix
    contents: "DICM"
  - id: elements
    type: data_element
    repeat: eos

types:
  data_element:
    seq:
      - id: tag_group
        type: u2
      - id: tag_element
        type: u2
      - id: vr
        type: str
        size: 2
        encoding: ASCII
        if: _parent.is_explicit_vr
      - id: reserved
        size: 2
        if: _parent.is_explicit_vr and (vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN")
      - id: value_length
        type: u4
        if: _parent.is_explicit_vr and (vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN")
      - id: value_length_implicit
        type: u2
        if: _parent.is_explicit_vr and not (vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN")
      - id: value_length_implicit_2
        type: u4
        if: not _parent.is_explicit_vr
      - id: value
        size: value_length_explicit_or_implicit

instances:
  is_explicit_vr:
    value: prefix == "DICM"
  value_length_explicit_or_implicit:
    value: (value_length if _parent.is_explicit_vr and (vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN") else (value_length_implicit if _parent.is_explicit_vr else value_length_implicit_2))