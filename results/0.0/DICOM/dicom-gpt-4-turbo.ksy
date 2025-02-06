meta:
  id: dicom
  file-extension: dcm
  endian: le
  title: Digital Imaging and Communications in Medicine (DICOM)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  DICOM (Digital Imaging and Communications in Medicine) is a standard for handling, storing, printing, and transmitting information in medical imaging.

seq:
  - id: preamble
    size: 128
    doc: The preamble is reserved for file meta information and is typically set to 0.
  - id: prefix
    size: 4
    contents: 'DICM'
    doc: The DICM prefix indicates that what follows is a DICOM dataset.
  - id: file_meta_information
    type: file_meta_information_group
  - id: data_sets
    type: data_element
    repeat: eos

types:
  file_meta_information_group:
    seq:
      - id: elements
        type: data_element
        repeat: until
        repeat-until: _.tag.group != 0x0002

  data_element:
    seq:
      - id: tag
        type: tag
      - id: vr
        type: vr
        if: tag.group != 0xfffe
      - id: reserved
        size: 2
        if: vr.is_long
      - id: value_length
        type: u4
        if: vr.is_long
      - id: value_length_short
        type: u2
        if: not vr.is_long
      - id: value_field
        size: 
          if: vr.is_long
          then: value_length
          else: value_length_short
        type:
          switch-on: vr.code
          cases:
            '"OB"': ob_value
            '"OW"': ow_value
            '"SQ"': sequence_of_items
            '"UN"': un_value
            '"UT"': ut_value
            '"UI"': ui_value
            '"SH"': sh_value
            '"DA"': da_value
            '"TM"': tm_value
            '"PN"': pn_value
            '"US"': us_value
            '"SS"': ss_value
            '"UL"': ul_value
            '"SL"': sl_value
            '"FL"': fl_value
            '"FD"': fd_value
            '"AT"': at_value
            '"IS"': is_value
            '"DS"': ds_value
            '"AE"': ae_value
            '"CS"': cs_value
            '"LO"': lo_value
            '"ST"': st_value
            '"LT"': lt_value
            '"AS"': as_value
            '"DT"': dt_value

  tag:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2

  vr:
    seq:
      - id: code
        type: str
        size: 2
    instances:
      is_long:
        value: code in ["OB", "OW", "SQ", "UN", "UT"]

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

  sequence_of_items:
    seq:
      - id: items
        type: data_element
        repeat: eos

  un_value:
    seq:
      - id: value
        type: u1
        repeat: eos

  ut_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  ui_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  sh_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  da_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  tm_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  pn_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  us_value:
    seq:
      - id: value
        type: u2

  ss_value:
    seq:
      - id: value
        type: s2

  ul_value:
    seq:
      - id: value
        type: u4

  sl_value:
    seq:
      - id: value
        type: s4

  fl_value:
    seq:
      - id: value
        type: f4

  fd_value:
    seq:
      - id: value
        type: f8

  at_value:
    seq:
      - id: value
        type: u2
        repeat: 2

  is_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  ds_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  ae_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  cs_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  lo_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  st_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  lt_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  as_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true

  dt_value:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true