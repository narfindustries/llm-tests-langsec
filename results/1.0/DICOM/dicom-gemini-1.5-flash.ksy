meta:
  id: dicom
  endian: be
types:
  dicom_element:
    seq:
      - id: tag
        type: u2
      - id: vr
        type: str
        size: 2
        encoding: ascii
      - id: length
        type: u4
      - id: value
        type: bytes
        size: length
  dicom_file:
    seq:
      - id: preamble
        type: bytes
        size: 128
      - id: dicom_prefix
        type: str
        size: 4
        encoding: ascii
      - id: meta_group_length
        type: u4
      - id: meta_elements
        type: dicom_element
        repeat: eos
      - id: data_elements
        type: dicom_element
        repeat: eos

