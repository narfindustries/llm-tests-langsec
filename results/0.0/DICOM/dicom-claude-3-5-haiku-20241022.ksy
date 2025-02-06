meta:
  id: dicom
  file-extension: dcm
  endian: le

seq:
  - id: preamble
    size: 128
  - id: dicom_prefix
    contents: [0x44, 0x49, 0x43, 0x4D]
  
  - id: file_meta_information_group
    type: file_meta_information
  
  - id: dataset
    type: dataset_sequence

types:
  file_meta_information:
    seq:
      - id: group_length
        type: attribute
      - id: transfer_syntax_uid
        type: attribute
      - id: implementation_class_uid
        type: attribute
      - id: implementation_version_name
        type: attribute
      - id: source_application_entity_title
        type: attribute
      - id: private_information_creator_uid
        type: attribute
      - id: private_information
        type: attribute
        
  dataset_sequence:
    seq:
      - id: attributes
        type: attribute
        repeat: until
        repeat-until: _io.is_eof
  
  attribute:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2
      - id: vr
        type: str
        encoding: ASCII
        size: 2
      - id: length
        type: u4
      - id: value
        type:
          switch-on: vr
          cases:
            '"AE"': str_type
            '"AS"': str_type
            '"AT"': tag_type
            '"CS"': str_type
            '"DA"': date_type
            '"DS"': decimal_type
            '"DT"': datetime_type
            '"FL"': float_type
            '"FD"': double_type
            '"IS"': integer_type
            '"LO"': str_type
            '"LT"': str_type
            '"PN"': person_name_type
            '"SH"': str_type
            '"SL"': s4
            '"SS"': s2
            '"ST"': str_type
            '"TM"': time_type
            '"UI"': uid_type
            '"UL"': u4
            '"US"': u2
            _: raw_type
  
  str_type:
    seq:
      - id: content
        type: str
        encoding: UTF-8
        size-eos: true
  
  tag_type:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2
  
  date_type:
    seq:
      - id: content
        type: str
        encoding: ASCII
        size: 8
  
  time_type:
    seq:
      - id: content
        type: str
        encoding: ASCII
        size: 6
  
  datetime_type:
    seq:
      - id: content
        type: str
        encoding: ASCII
        size: 14
  
  decimal_type:
    seq:
      - id: content
        type: str
        encoding: UTF-8
        size-eos: true
  
  integer_type:
    seq:
      - id: content
        type: str
        encoding: UTF-8
        size-eos: true
  
  float_type:
    seq:
      - id: content
        type: f4
  
  double_type:
    seq:
      - id: content
        type: f8
  
  uid_type:
    seq:
      - id: content
        type: str
        encoding: UTF-8
        size-eos: true
  
  person_name_type:
    seq:
      - id: content
        type: str
        encoding: UTF-8
        size-eos: true
  
  raw_type:
    seq:
      - id: content
        type: bytes
        size: _parent.length