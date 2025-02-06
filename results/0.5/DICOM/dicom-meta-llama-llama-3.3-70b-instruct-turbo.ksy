meta:
  id: dicom
  title: Digital Imaging and Communications in Medicine
  endian: be

types:
  dicom_data_set:
    seq:
      - id: data_elements
        type: data_element
        repeat: until
        until: tag == 0x00000000

  data_element:
    seq:
      - id: tag
        size: 4
      - id: vr
        size: 2
        type: str
        encoding: ascii
      - id: length
        size: 2
      - id: value
        size: length
        type:
          switch-on: vr
          cases:
            AE: application_entity
            AS: age_string
            AT: attribute_tag
            CS: code_string
            DA: date
            DS: decimal_string
            DT: date_time
            FL: floating_point_single
            FD: floating_point_double
            IS: integer_string
            LO: long_string
            LT: long_text
            OB: other_byte_string
            OD: other_double_string
            OF: other_float_string
            OL: other_long
            OW: other_word
            PN: person_name
            SH: short_string
            SL: string
            SQ: sequence_of_items
            SS: signed_short
            ST: short_text
            TM: time
            UI: unique_identifier
            UL: unsigned_long
            UN: unknown
            UR: uri
            US: unsigned_short
            UV: unsigned_value

  application_entity:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  age_string:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  attribute_tag:
    seq:
      - id: value
        size: 4

  code_string:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  date:
    seq:
      - id: value
        size: 8
        type: str
        encoding: ascii

  decimal_string:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  date_time:
    seq:
      - id: value
        size: 26
        type: str
        encoding: ascii

  floating_point_single:
    seq:
      - id: value
        size: 4
        type: f4

  floating_point_double:
    seq:
      - id: value
        size: 8
        type: f8

  integer_string:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  long_string:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  long_text:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  other_byte_string:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  other_double_string:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  other_float_string:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  other_long:
    seq:
      - id: value
        size: 4
        type: u4

  other_word:
    seq:
      - id: value
        size: 2
        type: u2

  person_name:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  sequence_of_items:
    seq:
      - id: items
        type: item
        repeat: eos

  item:
    seq:
      - id: delimitation_item
        size: 4
        type: u4
      - id: data_set
        type: dicom_data_set

  short_string:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  short_text:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  time:
    seq:
      - id: value
        size: 14
        type: str
        encoding: ascii

  unique_identifier:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  unsigned_long:
    seq:
      - id: value
        size: 4
        type: u4

  unknown:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  uri:
    seq:
      - id: value
        size-eos: true
        type: str
        encoding: ascii

  unsigned_short:
    seq:
      - id: value
        size: 2
        type: u2

  unsigned_value:
    seq:
      - id: value
        size: 4
        type: u4

  signed_short:
    seq:
      - id: value
        size: 2
        type: s2

seq:
  - id: preamble
    size: 128
  - id: prefix
    size: 4
    type: str
    encoding: ascii
  - id: dicom_data_set
    type: dicom_data_set