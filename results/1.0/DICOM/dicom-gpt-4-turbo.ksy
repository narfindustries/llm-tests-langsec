meta:
  id: dicom
  file-extension: dcm
  endian: le
seq:
  - id: preamble
    size: 128
  - id: dicom_prefix
    contents: 'DICM'
  - id: elements
    type: data_element
    repeat: eos
types:
  data_element:
    seq:
      - id: group_number
        type: u2
      - id: element_number
        type: u2
      - id: vr
        type: str
        size: 2
        encoding: ASCII
      - id: reserved
        type: u2
        if: has_explicit_vr
      - id: value_length
        type: u4
        if: has_explicit_vr
      - id: implicit_value_length
        type: u4
        if: is_implicit_vr
      - id: value
        size: value_length_calculated
    instances:
      has_explicit_vr:
        value: |
          vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN" or
          vr == "AE" or vr == "AS" or vr == "AT" or vr == "CS" or vr == "DA" or vr == "DS" or
          vr == "DT" or vr == "IS" or vr == "LO" or vr == "LT" or vr == "PN" or vr == "SH" or
          vr == "SL" or vr == "SS" or vr == "ST" or vr == "TM" or vr == "UI" or vr == "UL" or vr == "US"
      is_implicit_vr:
        value: not has_explicit_vr
      value_length_calculated:
        value: "_io.read_u4() if is_implicit_vr else value_length"