meta:
  id: dicom
  file-extension: dicom
  endian: le
  license: CC0-1.0
seq:
  - id: preamble
    size: 128
  - id: magic
    contents: [0x44, 0x49, 0x43, 0x4D] # "DICM"
  - id: data_elements
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
        encoding: ASCII
        size: 2
      - id: reserved
        size: 2
        if: vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN"
      - id: value_length
        type: u4
      - id: value
        size: value_length
        type:
          switch-on: vr
          cases:
            '"AE", "AS", "AT", "CS", "DA", "DS", "DT", "FL", "FD", "IS", "LO", "LT", "PN", "SH", "SL", "SS", "ST", "TM", "UI", "UL", "US"': strz
            '"OB", "OW", "OF", "SQ", "UT", "UN"': byte_array

  byte_array:
    seq:
      - id: data
        size-eos: true
