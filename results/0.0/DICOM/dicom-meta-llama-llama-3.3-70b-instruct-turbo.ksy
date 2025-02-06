meta:
  id: dicom
  endian: be

seq:
  - id: preamble
    size: 128
    type: bytes

  - id: prefix
    size: 4
    type: str
    encoding: ascii
    size-eos: false

  - id: meta_info_group
    type: meta_info_group

types:
  meta_info_group:
    seq:
      - id: transfer_syntax_uid
        size: 2
        type: str
        encoding: ascii

      - id: implicit_vr_little_endian
        size: 2
        type: bytes

      - id: source_app_entity_title
        size: 16
        type: str
        encoding: ascii
        pad-right: yes

      - id: sending_app_entity_title
        size: 16
        type: str
        encoding: ascii
        pad-right: yes

      - id: receiving_app_entity_title
        size: 16
        type: str
        encoding: ascii
        pad-right: yes

      - id: private_1
        size: 32
        type: bytes

      - id: private_2
        size: 32
        type: bytes

      - id: file_meta_info_version
        size: 2
        type: bytes

      - id: media_storage_sop_instance_uid
        size: 32
        type: str
        encoding: ascii
        pad-right: yes

      - id: media_storage_sop_class_uid
        size: 32
        type: str
        encoding: ascii
        pad-right: yes

      - id: implementation_class_uid
        size: 32
        type: str
        encoding: ascii
        pad-right: yes

      - id: transfer_syntax_uid_2
        size: 32
        type: str
        encoding: ascii
        pad-right: yes

      - id: implementation_version_name
        size: 16
        type: str
        encoding: ascii
        pad-right: yes

      - id: source_app_entity_title_2
        size: 16
        type: str
        encoding: ascii
        pad-right: yes

      - id: private_3
        size: 32
        type: bytes

  data_set:
    seq:
      - id: tag
        size: 4
        type: uint

      - id: vr
        size: 2
        type: str
        encoding: ascii

      - id: length
        size: 2
        type: uint

      - id: value
        size: = length
        type: bytes

  patient_module:
    seq:
      - id: patient_name
        type: person_name

      - id: patient_id
        type: str
        encoding: ascii
        pad-right: yes

      - id: patient_birth_date
        type: date

      - id: patient_sex
        type: str
        encoding: ascii
        enum: [M, F, O]

      - id: patient_age
        type: str
        encoding: ascii

  study_module:
    seq:
      - id: study_instance_uid
        type: str
        encoding: ascii
        pad-right: yes

      - id: study_date
        type: date

      - id: study_time
        type: time

      - id: referring_physician_name
        type: person_name

      - id: study_id
        type: str
        encoding: ascii
        pad-right: yes

      - id: study_description
        type: str
        encoding: ascii
        pad-right: yes

  series_module:
    seq:
      - id: series_instance_uid
        type: str
        encoding: ascii
        pad-right: yes

      - id: series_number
        type: uint

      - id: series_description
        type: str
        encoding: ascii
        pad-right: yes

      - id: series_date
        type: date

      - id: series_time
        type: time

      - id: modality
        type: str
        encoding: ascii
        enum: [CT, MR, US, etc.]

  image_module:
    seq:
      - id: sop_instance_uid
        type: str
        encoding: ascii
        pad-right: yes

      - id: image_position_patient
        type: float
        repeat: 3

      - id: image_orientation_patient
        type: float
        repeat: 6

      - id: image_type
        type: str
        encoding: ascii
        pad-right: yes

      - id: samples_per_pixel
        type: uint

      - id: photometric_interpretation
        type: str
        encoding: ascii
        enum: [MONOCHROME1, MONOCHROME2, RGB, etc.]

      - id: rows
        type: uint

      - id: columns
        type: uint

      - id: pixel_data
        type: bytes

  person_name:
    seq:
      - id: family_name
        type: str
        encoding: ascii
        pad-right: yes

      - id: given_name
        type: str
        encoding: ascii
        pad-right: yes

      - id: middle_name
        type: str
        encoding: ascii
        pad-right: yes

      - id: prefix
        type: str
        encoding: ascii
        pad-right: yes

      - id: suffix
        type: str
        encoding: ascii
        pad-right: yes

  date:
    seq:
      - id: year
        size: 4
        type: uint

      - id: month
        size: 2
        type: uint

      - id: day
        size: 2
        type: uint

  time:
    seq:
      - id: hour
        size: 2
        type: uint

      - id: minute
        size: 2
        type: uint

      - id: second
        size: 2
        type: uint

  float:
    seq:
      - id: value
        size: 4
        type: float

  uint:
    seq:
      - id: value
        size: 4
        type: uint

  bytes:
    seq:
      - id: value
        type: byte
        repeat: expr

  str:
    seq:
      - id: value
        type: byte
        repeat: expr
        encoding: ascii

  byte:
    seq:
      - id: value
        size: 1
        type: uint

  expr:
    seq:
      - id: value
        type: uint

instances:
  meta_info_group:
    pos: 128
    type: meta_info_group

  data_set:
    pos: 130
    type: data_set
    repeat: until_after

  patient_module:
    pos: 132
    type: patient_module

  study_module:
    pos: 134
    type: study_module

  series_module:
    pos: 136
    type: series_module

  image_module:
    pos: 138
    type: image_module

  person_name:
    pos: 140
    type: person_name

  date:
    pos: 142
    type: date

  time:
    pos: 144
    type: time

  float:
    pos: 146
    type: float

  uint:
    pos: 148
    type: uint

  bytes:
    pos: 150
    type: bytes

  str:
    pos: 152
    type: str
    encoding: ascii

  byte:
    pos: 154
    type: byte

  expr:
    pos: 156
    type: expr