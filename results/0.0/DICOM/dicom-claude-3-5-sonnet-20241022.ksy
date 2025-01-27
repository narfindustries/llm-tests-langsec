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
        if: is_explicit_vr_with_length
      - id: length
        type:
          switch-on: is_explicit_vr
          cases:
            true: length_field
            false: u4
      - id: data
        size: data_size
        type:
          switch-on: vr_or_implicit
          cases:
            '"AE"': str
            '"AS"': str
            '"AT"': u2
            '"CS"': str
            '"DA"': str
            '"DS"': str
            '"DT"': str
            '"FL"': f4
            '"FD"': f8
            '"IS"': str
            '"LO"': str
            '"LT"': str
            '"OB"': bytes
            '"OD"': bytes
            '"OF"': bytes
            '"OW"': bytes
            '"PN"': str
            '"SH"': str
            '"SL"': s4
            '"SQ"': sequence
            '"SS"': s2
            '"ST"': str
            '"TM"': str
            '"UI"': str
            '"UL"': u4
            '"UN"': bytes
            '"US"': u2
            '"UT"': str
            _: bytes

    instances:
      is_explicit_vr:
        value: '_root.is_explicit_vr'
      is_explicit_vr_with_length:
        value: 'is_explicit_vr and vr_needs_additional_length'
      vr_needs_additional_length:
        value: 'vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN"'
      vr_or_implicit:
        value: 'is_explicit_vr ? vr : "UN"'
      data_size:
        value: 'length.value'

  length_field:
    seq:
      - id: value
        type:
          switch-on: _parent.vr_needs_additional_length
          cases:
            true: u4
            false: u2

  sequence:
    seq:
      - id: items
        type: sequence_item
        repeat: eos

  sequence_item:
    seq:
      - id: tag
        contents: [0xfe, 0xff, 0x00, 0xe0]
      - id: length
        type: u4
      - id: elements
        type: element
        repeat: eos
        if: length != 0

instances:
  is_explicit_vr:
    value: true