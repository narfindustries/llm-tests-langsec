meta:
  id: dicom
  title: DICOM
  file-extension: dcm
  endian: be

seq:
  - id: preamble
    size: 128
    type: str

  - id: prefix
    size: 4
    type: str

  - id: transfer-syntax-uid
    size: 2
    type: u2

  - id: group-0002-elements
    type: group_0002_elements
    repeat: expr
    repeat-expr: >= 12

types:
  group_0002_elements:
    seq:
      - id: tag
        size: 2
        type: u2

      - id: vr
        size: 2
        type: str

      - id: length
        size: 2
        type: u2

      - id: value
        size: of: length
        type: str