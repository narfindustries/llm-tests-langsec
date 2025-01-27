meta:
  id: nitf
  file-extension: nitf
  endian: big
  imports:
    - ./nitf-meta-llama-llama-3
seq:
  - id: file-header
    type: str
    size: 4
    enum: [NITF, nift]
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
  - id: file-creation-date
    type: str
    size: 14
  - id: file-title
    type: str
    size: 80
  - id: file-security-control-and-handling
    type: str
    size: 15
  - id: file-file-name
    type: str
    size: 24
  - id: file-file-part-type
    type: str
    size: 2
  - id: file-file-part-version
    type: str
    size: 2
  - id: file-file-part-file-date
    type: str
    size: 14
  - id: file-file-part-file-title
    type: str
    size: 80
  - id: file-file-part-security-classification
    type: str
    size: 1
  - id: file-file-part-encryption
    type: str
    size: 1
  - id: file-file-part-file-control
    type: str
    size: 2
  - id: file-file-part-message-file-date
    type: str
    size: 14
  - id: file-file-part-message-file-title
    type: str
    size: 80
  - id: file-file-part-message-security-classification
    type: str
    size: 1
  - id: file-file-part-message-encryption
    type: str
    size: 1
  - id: file-file-part-message-file-control
    type: str
    size: 2
  - id: file-file-part-message-file-name
    type: str
    size: 24
  - id: file-file-part-message-file-part-type
    type: str
    size: 2
  - id: file-file-part-message-file-part-version
    type: str
    size: 2
  - id: file-file-part-message-file-part-file-date
    type: str
    size: 14
  - id: file-file-part-message-file-part-file-title
    type: str
    size: 80
  - id: file-file-part-message-file-part-security-classification
    type: str
    size: 1
  - id: file-file-part-message-file-part-encryption
    type: str
    size: 1
  - id: file-file-part-message-file-part-file-control
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-date
    type: str
    size: 14
  - id: file-file-part-message-file-part-message-file-title
    type: str
    size: 80
  - id: file-file-part-message-file-part-message-security-classification
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-encryption
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-control
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-name
    type: str
    size: 24
  - id: file-file-part-message-file-part-message-file-part-type
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-version
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-file-date
    type: str
    size: 14
  - id: file-file-part-message-file-part-message-file-part-file-title
    type: str
    size: 80
  - id: file-file-part-message-file-part-message-file-part-security-classification
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-encryption
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-file-control
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-message-file-date
    type: str
    size: 14
  - id: file-file-part-message-file-part-message-file-part-message-file-title
    type: str
    size: 80
  - id: file-file-part-message-file-part-message-file-part-message-security-classification
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-message-encryption
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-message-file-control
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-message-file-name
    type: str
    size: 24
  - id: file-file-part-message-file-part-message-file-part-message-file-part-type
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-message-file-part-version
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-message-file-part-file-date
    type: str
    size: 14
  - id: file-file-part-message-file-part-message-file-part-message-file-part-file-title
    type: str
    size: 80
  - id: file-file-part-message-file-part-message-file-part-message-file-part-security-classification
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-message-file-part-encryption
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-message-file-part-file-control
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-date
    type: str
    size: 14
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-title
    type: str
    size: 80
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-security-classification
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-encryption
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-control
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-name
    type: str
    size: 24
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-type
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-version
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-file-date
    type: str
    size: 14
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-file-title
    type: str
    size: 80
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-security-classification
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-encryption
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-file-control
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-message-file-date
    type: str
    size: 14
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-message-file-title
    type: str
    size: 80
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-message-security-classification
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-message-encryption
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-message-file-control
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-message-file-name
    type: str
    size: 24
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-message-file-part-type
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-message-file-part-version
    type: str
    size: 2
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-message-file-part-file-date
    type: str
    size: 14
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-message-file-part-file-title
    type: str
    size: 80
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-message-file-part-security-classification
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-message-file-part-encryption
    type: str
    size: 1
  - id: file-file-part-message-file-part-message-file-part-message-file-part-message-file-part-message-file-part-file-control
    type: str
    size: 2