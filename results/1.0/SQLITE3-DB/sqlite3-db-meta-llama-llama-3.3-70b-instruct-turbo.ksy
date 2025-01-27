meta:
  id: sqlite3-db
  file-extension: db
  endian: le
  encoding: UTF-8
seq:
  - id: magic
    type: str
    size: 16
    encoding: ASCII
  - id: page-size
    type: u2
  - id: write-version
    type: u1
  - id: read-version
    type: u1
  - id: reserved-space
    type: u1
  - id: max-embedded-payload-fraction
    type: u1
  - id: min-embedded-payload-fraction
    type: u1
  - id: leaf-rotation
    type: u1
  - id: file-format-version
    type: u1
types:
  data-page:
    seq:
      - id: page-type
        type: u1
      - id: first-fragment-offset
        type: u2
      - id: data
        type: bytes
        size: ptr -> page-size
  index-page:
    seq:
      - id: page-type
        type: u1
      - id: first-fragment-offset
        type: u2
      - id: index-entries
        type: index-entry
        repeat: expr -> (page-size - first-fragment-offset) / 12
  index-entry:
    seq:
      - id: length
        type: u1
      - id: data
        type: bytes
        size: length