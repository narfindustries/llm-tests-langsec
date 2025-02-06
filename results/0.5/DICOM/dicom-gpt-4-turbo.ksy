meta:
  id: dicom
  file-extension: dcm
  endian: le
seq:
  - id: preamble
    size: 128
  - id: prefix
    contents: [0x44, 0x49, 0x43, 0x4D]
  - id: file_meta_information
    type: data_set
  - id: image_data_set
    type: data_set
types:
  data_element_explicit:
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
        if: vr in ['OB', 'OW', 'OF', 'SQ', 'UT', 'UN']
      - id: value_length
        type: u4
      - id: value_field
        size: value_length
  data_element_implicit:
    seq:
      - id: group_number
        type: u2
      - id: element_number
        type: u2
      - id: value_length
        type: u4
      - id: value_field
        size: value_length
  data_set:
    seq:
      - id: elements
        type:
          switch-on: is_explicit_vr
          cases:
            true: data_element_explicit
            false: data_element_implicit
        repeat: until
        repeat-until: _.group_number == 0xFFFE and _.element_number == 0xE0DD
instances:
  is_explicit_vr:
    value: file_meta_information.elements[0].vr == 'UL'