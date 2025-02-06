meta:
  id: dicom
  endian: le
  bit-endian: le

types:
  dicom_header:
    seq:
      - id: preamble
        size: 128
      - id: magic
        contents: [0x44, 0x49, 0x43, 0x4D]  # "DICM"

  attribute:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2
      - id: vr
        type: str
        size: 2
        encoding: ASCII
      - id: length
        type: u4
      - id: value
        size-eos: true
        type:
          switch-on: vr
          cases:
            '"AE"': ae_value
            '"AS"': as_value
            '"AT"': at_value
            '"CS"': cs_value
            '"DA"': da_value
            '"DS"': ds_value
            '"DT"': dt_value
            '"FL"': fl_value
            '"FD"': fd_value
            '"IS"': is_value
            '"LO"': lo_value
            '"LT"': lt_value
            '"OB"': ob_value
            '"OD"': od_value
            '"OF"': of_value
            '"OL"': ol_value
            '"OW"': ow_value
            '"PN"': pn_value
            '"SH"': sh_value
            '"SL"': sl_value
            '"SQ"': sq_value
            '"SS"': ss_value
            '"ST"': st_value
            '"TM"': tm_value
            '"UI"': ui_value
            '"UL"': ul_value
            '"UN"': un_value
            '"US"': us_value
            '"UT"': ut_value

  ae_value:
    seq:
      - id: text
        type: str
        encoding: ASCII
        size-eos: true

  as_value:
    seq:
      - id: age
        type: str
        size: 4
        encoding: ASCII

  at_value:
    seq:
      - id: tag_group
        type: u2
      - id: tag_element
        type: u2

  cs_value:
    seq:
      - id: code
        type: str
        encoding: ASCII
        size-eos: true

  da_value:
    seq:
      - id: date
        type: str
        size: 8
        encoding: ASCII

  ds_value:
    seq:
      - id: decimal_string
        type: str
        encoding: ASCII
        size-eos: true

  dt_value:
    seq:
      - id: datetime
        type: str
        encoding: ASCII
        size-eos: true

  fl_value:
    seq:
      - id: float_value
        type: f4

  fd_value:
    seq:
      - id: double_value
        type: f8

  is_value:
    seq:
      - id: integer_string
        type: str
        encoding: ASCII
        size-eos: true

  lo_value:
    seq:
      - id: long_string
        type: str
        encoding: ASCII
        size-eos: true

  lt_value:
    seq:
      - id: long_text
        type: str
        encoding: ASCII
        size-eos: true

  ob_value:
    seq:
      - id: other_byte
        type: bytes
        size-eos: true

  od_value:
    seq:
      - id: other_double
        type: bytes
        size-eos: true

  of_value:
    seq:
      - id: other_float
        type: bytes
        size-eos: true

  ol_value:
    seq:
      - id: other_long
        type: bytes
        size-eos: true

  ow_value:
    seq:
      - id: other_word
        type: bytes
        size-eos: true

  pn_value:
    seq:
      - id: patient_name
        type: str
        encoding: ASCII
        size-eos: true

  sh_value:
    seq:
      - id: short_string
        type: str
        encoding: ASCII
        size-eos: true

  sl_value:
    seq:
      - id: signed_long
        type: s4

  sq_value:
    seq:
      - id: sequence
        type: attribute
        repeat: eos

  ss_value:
    seq:
      - id: signed_short
        type: s2

  st_value:
    seq:
      - id: short_text
        type: str
        encoding: ASCII
        size-eos: true

  tm_value:
    seq:
      - id: time
        type: str
        size: 8
        encoding: ASCII

  ui_value:
    seq:
      - id: unique_identifier
        type: str
        encoding: ASCII
        size-eos: true

  ul_value:
    seq:
      - id: unsigned_long
        type: u4

  un_value:
    seq:
      - id: unknown
        type: bytes
        size-eos: true

  us_value:
    seq:
      - id: unsigned_short
        type: u2

  ut_value:
    seq:
      - id: unlimited_text
        type: str
        encoding: ASCII
        size-eos: true

seq:
  - id: header
    type: dicom_header
  - id: attributes
    type: attribute
    repeat: eos