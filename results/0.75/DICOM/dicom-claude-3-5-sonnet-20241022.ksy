meta:
  id: dicom
  title: DICOM Medical Image Format
  file-extension: dcm
  endian: le

seq:
  - id: preamble
    size: 128
  - id: magic
    contents: "DICM"
  - id: data_elements
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
        if: not is_implicit_vr
      - id: reserved
        type: u2
        if: is_explicit_vr_long
      - id: length
        type: u4
        if: is_implicit_vr
      - id: length_explicit
        type: length_field
        if: not is_implicit_vr
      - id: data
        size: data_size
        type:
          switch-on: tag
          cases:
            0x00020010: transfer_syntax
            0x00080060: modality
            0x00100010: patient_name
            0x00100020: patient_id
            0x00100030: patient_birth_date
            0x00100040: patient_sex
            0x0020000d: study_instance_uid
            0x0020000e: series_instance_uid
            0x00200010: study_id
            0x00200011: series_number
            0x00280002: samples_per_pixel
            0x00280010: rows
            0x00280011: columns
            0x00280100: bits_allocated
            0x00280101: bits_stored
            0x00280102: high_bit
            0x00280103: pixel_representation
            _: raw_data

    instances:
      tag:
        value: (tag_group << 16) | tag_element
      is_implicit_vr:
        value: _root.transfer_syntax_implicit
      is_explicit_vr_long:
        value: vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN"
      data_size:
        value: length_explicit.value
        if: not is_implicit_vr
      data_size_implicit:
        value: length
        if: is_implicit_vr

  length_field:
    seq:
      - id: value
        type:
          switch-on: _parent.is_explicit_vr_long
          cases:
            true: u4
            false: u2

  transfer_syntax:
    seq:
      - id: value
        type: strz
        encoding: ASCII

  modality:
    seq:
      - id: value
        type: str
        size: _parent.data_size
        encoding: ASCII

  patient_name:
    seq:
      - id: value
        type: str
        size: _parent.data_size
        encoding: ASCII

  patient_id:
    seq:
      - id: value
        type: str
        size: _parent.data_size
        encoding: ASCII

  patient_birth_date:
    seq:
      - id: value
        type: str
        size: _parent.data_size
        encoding: ASCII

  patient_sex:
    seq:
      - id: value
        type: str
        size: _parent.data_size
        encoding: ASCII

  study_instance_uid:
    seq:
      - id: value
        type: str
        size: _parent.data_size
        encoding: ASCII

  series_instance_uid:
    seq:
      - id: value
        type: str
        size: _parent.data_size
        encoding: ASCII

  study_id:
    seq:
      - id: value
        type: str
        size: _parent.data_size
        encoding: ASCII

  series_number:
    seq:
      - id: value
        type: str
        size: _parent.data_size
        encoding: ASCII

  samples_per_pixel:
    seq:
      - id: value
        type: u2

  rows:
    seq:
      - id: value
        type: u2

  columns:
    seq:
      - id: value
        type: u2

  bits_allocated:
    seq:
      - id: value
        type: u2

  bits_stored:
    seq:
      - id: value
        type: u2

  high_bit:
    seq:
      - id: value
        type: u2

  pixel_representation:
    seq:
      - id: value
        type: u2

  raw_data:
    seq:
      - id: value
        size: _parent.data_size

instances:
  transfer_syntax_implicit:
    value: true