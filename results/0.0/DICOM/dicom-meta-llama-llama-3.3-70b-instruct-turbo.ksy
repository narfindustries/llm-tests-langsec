meta:
  id: dicom
  file-extension: dcm
  endian: be

seq:
  - id: preamble
    size: 128

  - id: prefix
    size: 4
    type: str
    encoding: ascii

  - id: transfer-syntax-uid
    size: 2
    type: str
    encoding: ascii

  - id: group-0002-elements
    type: group_0002_elements
    repeat: expr

seq:
  - id: group-0002-elements
    type:
      - id: element
        size: 8
        type: str
        encoding: ascii
      - id: length
        size: 2
        type: uint16
      - id: value
        size: length
        type: str
        encoding: ascii

types:
  group_0002_elements:
    seq:
      - id: element
        size: 8
        type: str
        encoding: ascii
      - id: length
        size: 2
        type: uint16
      - id: value
        size: length
        type: str
        encoding: ascii