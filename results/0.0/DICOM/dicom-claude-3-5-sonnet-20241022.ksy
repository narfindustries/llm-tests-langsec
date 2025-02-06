meta:
  id: dicom
  file-extension: dcm
  endian: le
  encoding: ascii

seq:
  - id: preamble
    size: 128
  - id: dicom_prefix
    contents: 'DICM'
  - id: elements
    type: element
    repeat: eos

types:
  element:
    instances:
      is_implicit_vr:
        value: 'tag_group != 0x0002'
      is_explicit_vr_and_long:
        value: 'not is_implicit_vr and ["OB", "OW", "OF", "SQ", "UN", "UT"].includes(vr)'
      vr_or_implicit:
        value: 'is_implicit_vr ? "UN" : vr'
      data_size:
        value: 'is_implicit_vr ? length : (is_explicit_vr_and_long ? length : _parent.length.length)'
    seq:
      - id: tag_group
        type: u2
      - id: tag_element
        type: u2
      - id: vr
        type: str
        size: 2
        encoding: ascii
        if: not is_implicit_vr
      - id: reserved
        type: u2
        if: is_explicit_vr_and_long
      - id: length
        type:
          switch-on: is_implicit_vr
          cases:
            true: u4
            false: length_field
      - id: data
        size: data_size
        type:
          switch-on: vr_or_implicit
          cases:
            '"AE"': strz
            '"AS"': strz
            '"AT"': u2
            '"CS"': strz
            '"DA"': strz
            '"DS"': strz
            '"DT"': strz
            '"FL"': f4
            '"FD"': f8
            '"IS"': strz
            '"LO"': strz
            '"LT"': strz
            '"OB"': raw_data
            '"OD"': raw_data
            '"OF"': raw_data
            '"OW"': raw_data
            '"PN"': strz
            '"SH"': strz
            '"SL"': s4
            '"SQ"': sequence
            '"SS"': s2
            '"ST"': strz
            '"TM"': strz
            '"UI"': strz
            '"UL"': u4
            '"UN"': raw_data
            '"US"': u2
            '"UT"': strz
            _: raw_data

  raw_data:
    seq:
      - id: content
        size-eos: true

  length_field:
    seq:
      - id: length
        type:
          switch-on: _parent.vr
          cases:
            '"OB"': u4
            '"OW"': u4
            '"OF"': u4
            '"SQ"': u4
            '"UN"': u4
            '"UT"': u4
            _: u2

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
        type: element
        repeat: until
        repeat-until: _io.pos >= _parent._io.pos + item_length
        if: item_length != 0