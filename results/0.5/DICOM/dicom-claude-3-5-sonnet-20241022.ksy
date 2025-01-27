meta:
  id: dicom
  file-extension: dcm
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
      - id: tag_group
        type: u2
      - id: tag_element
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
            true: length_field
            false: u4
      - id: data
        size: data_size
        type:
          switch-on: is_sequence
          cases:
            true: sequence_items
            false: raw_data

    instances:
      is_explicit_vr:
        value: true
      is_explicit_vr_with_reserved:
        value: 'is_explicit_vr and ["OB", "OW", "OF", "SQ", "UN", "UT"].includes(vr)'
      is_sequence:
        value: 'vr == "SQ"'
      data_size:
        value: length.value
      tag:
        value: '(tag_group << 16) | tag_element'

  length_field:
    seq:
      - id: length_value
        type:
          switch-on: is_reserved
          cases:
            true: u4
            false: u2
    instances:
      is_reserved:
        value: _parent.is_explicit_vr_with_reserved
      value:
        value: length_value

  sequence_items:
    seq:
      - id: items
        type: sequence_item
        repeat: until
        repeat-until: _.item_length == 0
        
  sequence_item:
    seq:
      - id: item_tag
        contents: [0xfe, 0xff, 0x00, 0xe0]
      - id: item_length
        type: u4
      - id: item_data
        size: item_length
        type: element
        if: item_length > 0
        repeat: eos

  raw_data:
    seq:
      - id: content
        size-eos: true