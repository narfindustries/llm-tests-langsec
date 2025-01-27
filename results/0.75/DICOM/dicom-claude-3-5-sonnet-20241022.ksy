meta:
  id: dicom
  file-extension: dcm
  endian: le

seq:
  - id: preamble
    size: 128
  - id: prefix
    size: 4
    contents: "DICM"
  - id: elements
    type: element
    repeat: eos

types:
  element:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2
      - id: vr
        type: str
        size: 2
        if: is_explicit_vr
      - id: reserved
        type: u2
        if: is_explicit_vr_with_reserved
      - id: length
        type:
          switch-on: is_explicit_vr
          cases:
            true: u2
            false: u4
        if: not is_explicit_vr_with_reserved
      - id: length_explicit_reserved
        type: u4
        if: is_explicit_vr_with_reserved
      - id: data
        size: data_size
    instances:
      tag:
        value: (group << 16) | element
      is_explicit_vr:
        value: true
      is_explicit_vr_with_reserved:
        value: >-
          is_explicit_vr and
          (vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or
           vr == "UT" or vr == "UN")
      data_size:
        value: >-
          is_explicit_vr_with_reserved ? length_explicit_reserved :
          is_explicit_vr ? length : length