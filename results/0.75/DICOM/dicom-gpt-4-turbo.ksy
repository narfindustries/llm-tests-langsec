meta:
  id: dicom
  file-extension: dcm
  endian: le
seq:
  - id: preamble
    size: 0x80
  - id: prefix
    contents: [0x44, 0x49, 0x43, 0x4d]
  - id: file_meta_information
    type: file_meta_information_group
  - id: data_sets
    type: data_set
    repeat: eos

types:
  file_meta_information_group:
    seq:
      - id: elements
        type: data_element
        repeat: until
        repeat-until: _.tag.group != 0x0002

  data_set:
    seq:
      - id: elements
        type: data_element
        repeat: eos

  data_element:
    seq:
      - id: tag
        type: data_element_tag
      - id: vr
        type: str
        size: 2
        encoding: ASCII
      - id: reserved
        size: 2
        if: >
          vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN"
      - id: length
        type: u4
      - id: value
        size: length
        type:
          switch-on: vr
          cases:
            '"OB"': ob_value
            '"OW"': ow_value
            '"OF"': of_value
            '"SQ"': sequence_of_items
            '"UT"': ut_value
            '"UN"': un_value

  data_element_tag:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2

  ob_value:
    seq:
      - id: value
        type: u1
        repeat: eos

  ow_value:
    seq:
      - id: value
        type: u2
        repeat: eos

  of_value:
    seq:
      - id: value
        type: f4
        repeat: eos

  ut_value:
    seq:
      - id: value
        type: str
        encoding: UTF-8

  un_value:
    seq:
      - id: value
        type: u1
        repeat: eos

  sequence_item:
    seq:
      - id: item_length
        type: u4
      - id: item_content
        size: item_length
        type: item
        if: item_length != 0xFFFFFFFF

  sequence_of_items:
    seq:
      - id: items
        type: sequence_item
        repeat: until
        repeat-until: _.item_length == 0xFFFFFFFE

  item:
    seq:
      - id: item_elements
        type: data_element
        repeat: eos