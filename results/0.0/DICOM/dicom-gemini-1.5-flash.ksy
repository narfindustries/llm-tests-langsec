types:
  dicom_element:
    seq:
      - id: tag
        type: u2be
      - id: vr
        type: str
        size: 2
        encoding: ASCII
      - id: vl
        type: u4be
      - id: value
        type: bytes
        size: vl

  dicom_file:
    seq:
      - id: file_meta_information
        type: dicom_element
      - id: dataset
        type: seq
        repeat: eos
        read: dicom_element

