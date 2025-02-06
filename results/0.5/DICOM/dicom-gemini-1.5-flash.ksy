# This is a simplified example and does not cover the full DICOM specification.
# A complete Kaitai Struct definition for DICOM would be extremely large and complex.

types:
  dicom_element:
    seq:
      - id: tag
        type: u4
      - id: vr
        type: str
        len: 2
      - id: length
        type: u4
      - id: value
        type: bytes
        len: expr
        len-expr: length

meta:
  id: dicom_file
  endian: be
  seq:
    - id: file_meta_information_group_length
      type: u4
    - id: file_meta_information_version
      type: u2
    - id: media_storage_sop_class_uid
      type: str
      len: 64
    - id: media_storage_sop_instance_uid
      type: str
      len: 64
    - id: transfer_syntax_uid
      type: str
      len: 64
    - id: implementation_class_uid
      type: str
      len: 64
    - id: implementation_version_name
      type: str
      len: 16
    - id: dataset
      type: dicom_element
      repeat: expr
      repeat-expr: until_eof

