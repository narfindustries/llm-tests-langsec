meta:
  id: dicom
  file-extension: dcm
  endian: le

seq:
  - id: preamble
    size: 128
  - id: prefix
    type: str
    size: 4
    encoding: ASCII
  - id: dataset
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
      - id: value_length
        type: length_field
      - id: value
        size: value_length.length
        type:
          switch-on: vr
          cases:
            '"AE"': str_value
            '"AS"': str_value
            '"CS"': str_value
            '"DA"': str_value
            '"DS"': str_value
            '"DT"': str_value
            '"IS"': str_value
            '"LO"': str_value
            '"LT"': str_value
            '"PN"': str_value
            '"SH"': str_value
            '"ST"': str_value
            '"TM"': str_value
            '"UI"': str_value
            '"UT"': str_value
            '"UN"': raw_value
            '"SQ"': sequence_value
            _: raw_value

  length_field:
    seq:
      - id: reserved
        type: u2
        if: is_explicit_vr
      - id: length
        type:
          switch-on: is_explicit_vr
          cases:
            true: u4
            false: u2
    instances:
      is_explicit_vr:
        value: _parent.vr != 'OB' and _parent.vr != 'OW' and _parent.vr != 'OF' and _parent.vr != 'SQ' and _parent.vr != 'UN' and _parent.vr != 'UT'

  str_value:
    seq:
      - id: content
        type: str
        size-eos: true
        encoding: ASCII

  raw_value:
    seq:
      - id: content
        size-eos: true

  sequence_value:
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
        type: data_element
        repeat: expr
        repeat-expr: item_length