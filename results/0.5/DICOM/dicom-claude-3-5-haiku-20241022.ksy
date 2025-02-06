meta:
  id: dicom
  file-extension: dcm
  endian: le

seq:
  - id: file_preamble
    size: 128
  - id: dicom_prefix
    type: str
    size: 4
    encoding: ASCII
    
  - id: file_meta_information_group
    type:
      seq:
        - id: group_length
          type: u2
        - id: transfer_syntax_uid
          type: dicom_attribute
        - id: implementation_class_uid
          type: dicom_attribute
        - id: implementation_version_name
          type: dicom_attribute
        
  - id: dataset
    type: dicom_dataset

types:
  dicom_dataset:
    seq:
      - id: attributes
        type: dicom_attribute
        repeat: until
        repeat-until: _io.is_eof

  dicom_attribute:
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
        type: attribute_value
        
  attribute_value:
    params:
      - id: vr
        type: str
      - id: length
        type: u4
    seq:
      - id: content
        type:
          switch-on: vr
          cases:
            '"AE"': str_attribute(length)  # Application Entity
            '"AS"': str_attribute(length)  # Age String
            '"AT"': tag_attribute(length)  # Attribute Tag
            '"CS"': str_attribute(length)  # Code String
            '"DA"': date_attribute(length)  # Date
            '"DS"': decimal_attribute(length)  # Decimal String
            '"DT"': datetime_attribute(length)  # Date Time
            '"FL"': float_attribute(length)  # Floating Point Single
            '"FD"': double_attribute(length)  # Floating Point Double
            '"IS"': integer_attribute(length)  # Integer String
            '"LO"': str_attribute(length)  # Long String
            '"LT"': str_attribute(length)  # Long Text
            '"OB"': binary_attribute(length)  # Other Byte
            '"OD"': binary_attribute(length)  # Other Double
            '"OF"': binary_attribute(length)  # Other Float
            '"OL"': binary_attribute(length)  # Other Long
            '"OW"': binary_attribute(length)  # Other Word
            '"PN"': person_name_attribute(length)  # Person Name
            '"SH"': str_attribute(length)  # Short String
            '"SL"': int32_attribute(length)  # Signed Long
            '"SQ"': sequence_attribute(length)  # Sequence of Items
            '"SS"': int16_attribute(length)  # Signed Short
            '"ST"': str_attribute(length)  # Short Text
            '"TM"': time_attribute(length)  # Time
            '"UI"': str_attribute(length)  # Unique Identifier
            '"UL"': uint32_attribute(length)  # Unsigned Long
            '"UN"': binary_attribute(length)  # Unknown
            '"US"': uint16_attribute(length)  # Unsigned Short
            '"UT"': str_attribute(length)  # Unlimited Text
            _: binary_attribute(length)

  str_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: str
        size: length
        encoding: UTF-8

  tag_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: u4
        repeat: expr
        repeat-expr: length / 4

  date_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: str
        size: length
        encoding: ASCII

  datetime_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: str
        size: length
        encoding: ASCII

  time_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: str
        size: length
        encoding: ASCII

  decimal_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: str
        size: length
        encoding: ASCII

  integer_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: str
        size: length
        encoding: ASCII

  float_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: f4

  double_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: f8

  int16_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: s2

  int32_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: s4

  uint16_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: u2

  uint32_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: u4

  binary_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        size: length

  person_name_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: value
        type: str
        size: length
        encoding: UTF-8

  sequence_attribute:
    params:
      - id: length
        type: u4
    seq:
      - id: items
        type: sequence_item
        repeat: eos

  sequence_item:
    seq:
      - id: item_delimiter
        type: u4
      - id: item_length
        type: u4
      - id: attributes
        type: dicom_dataset
        if: item_length > 0

enums:
  modality:
    0: 'CT'
    1: 'MR'
    2: 'US'
    3: 'XA'
    4: 'CR'
    5: 'RF'
    6: 'NM'
    7: 'PET'
    8: 'SC'
    9: 'OT'