meta:
  id: dicom
  file-extension: dcm
  endian: le
seq:
  - id: preamble
    size: 128
    doc: "DICOM file preamble (128 bytes)"
  - id: magic
    contents: "DICM"
    doc: "DICOM magic number"
  - id: elements
    type: dicom_element
    repeat: eos
types:
  dicom_element:
    seq:
      - id: tag
        type: u2
        repeat: expr
        repeat-expr: 2
        doc: "DICOM element tag (group and element)"
      - id: vr
        type: str
        size: 2
        doc: "Value Representation (VR)"
      - id: length
        type:
          switch-on: vr
          cases:
            '"OB"': u4
            '"OW"': u4
            '"SQ"': u4
            '"UN"': u4
            _: u2
        doc: "Length of element data"
      - id: value
        size: length
        doc: "Element value data"