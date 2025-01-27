meta:
  id: dicom
  title: DICOM (Digital Imaging and Communications in Medicine)
  file-extension: dcm
  license: MIT
  endian: le
  encoding: ASCII

seq:
  - id: preamble
    size: 128
    doc: Preamble (128 bytes, usually zeros)
  - id: prefix
    contents: "DICM"
    doc: Prefix identifying the file as DICOM
  - id: elements
    type: element
    repeat: eos
    doc: Sequence of DICOM elements

types:
  element:
    seq:
      - id: tag
        type: tag
        doc: Tag identifying the element
      - id: vr
        type: vr
        doc: Value Representation (VR) of the element
      - id: length
        type: u2
        doc: Length of the value field
      - id: value
        size: length
        doc: Value of the element

  tag:
    seq:
      - id: group
        type: u2
        doc: Group number of the tag
      - id: element
        type: u2
        doc: Element number within the group

  vr:
    seq:
      - id: vr_code
        type: str
        size: 2
        encoding: ASCII
        doc: Two-character VR code