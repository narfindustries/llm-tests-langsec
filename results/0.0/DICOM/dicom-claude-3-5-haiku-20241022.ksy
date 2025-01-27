meta:
  id: dicom
  file-extension: dcm
  endian: le
seq:
  - id: preamble
    size: 128
    doc: 128-byte preamble
  - id: magic
    contents: "DICM"
    doc: DICOM magic number
  - id: meta_elements
    type: meta_elements
    repeat: eos
types:
  meta_elements:
    seq:
      - id: tag
        type: u2
        repeat: expr
        repeat-expr: 2
        doc: DICOM tag (group and element)
      - id: vr
        type: str
        size: 2
        doc: Value Representation
      - id: length
        type: u2
        doc: Length of the data
      - id: data
        size: length
        doc: Actual data payload