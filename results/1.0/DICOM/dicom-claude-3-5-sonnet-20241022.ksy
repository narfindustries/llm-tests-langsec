meta:
  id: dicom
  file-extension: dcm
  endian: le

seq:
  - id: preamble
    size: 128
  - id: dicom_prefix
    contents: 'DICM'
  - id: meta_information
    type: meta_information_group
  - id: elements
    type: data_element
    repeat: eos

types:
  meta_information_group:
    seq:
      - id: transfer_syntax
        type: data_element

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
        if: not is_implicit_vr
      - id: length
        type:
          switch-on: vr
          cases:
            '"OB"': u4
            '"OW"': u4
            '"OF"': u4
            '"SQ"': u4
            '"UN"': u4
            '"UT"': u4
            _: u2
        if: not is_implicit_vr
      - id: length_implicit
        type: u4
        if: is_implicit_vr
      - id: value
        size: length_value
        type:
          switch-on: vr
          cases:
            '"AE"': str_value
            '"AS"': str_value
            '"AT"': tag_value
            '"CS"': str_value
            '"DA"': str_value
            '"DS"': str_value
            '"DT"': str_value
            '"FL"': f4
            '"FD"': f8
            '"IS"': str_value
            '"LO"': str_value
            '"LT"': str_value
            '"OB"': raw_bytes
            '"OD"': raw_bytes
            '"OF"': raw_bytes
            '"OW"': raw_bytes
            '"PN"': str_value
            '"SH"': str_value
            '"SL"': s4
            '"SQ"': sequence
            '"SS"': s2
            '"ST"': str_value
            '"TM"': str_value
            '"UI"': str_value
            '"UL"': u4
            '"UN"': raw_bytes
            '"US"': u2
            '"UT"': str_value
            _: raw_bytes

    instances:
      tag:
        value: (tag_group << 16) | tag_element
      is_implicit_vr:
        value: '_root.meta_information.transfer_syntax.value.as<str_value>.value == "1.2.840.10008.1.2"'
      length_value:
        value: 'is_implicit_vr ? length_implicit : length'

  str_value:
    seq:
      - id: value
        type: str
        size-eos: true
        encoding: ASCII

  raw_bytes:
    seq:
      - id: value
        size-eos: true

  tag_value:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2

  sequence:
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
      - id: elements
        type: data_element
        repeat: expr
        repeat-expr: item_length / 8