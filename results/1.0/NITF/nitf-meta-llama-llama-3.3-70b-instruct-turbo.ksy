meta:
  id: nitf
  title: NITF
  file-extension: nitf
  endianness: be
  encoding: ASCII
seq:
  - id: file-header
    type: str
    size: 4
    enum: [NITF]
  - id: file-version
    type: str
    size: 5
  - id: file-type
    type: str
    size: 2
  - id: file-security-classification
    type: str
    size: 1
  - id: file-control
    type: str
    size: 2
types:
  FileHeader:
    seq:
      - id: file-header
        type: str
        size: 4
        enum: [NITF]
      - id: file-version
        type: str
        size: 5
      - id: file-type
        type: str
        size: 2
      - id: file-security-classification
        type: str
        size: 1
      - id: file-control
        type: str
        size: 2