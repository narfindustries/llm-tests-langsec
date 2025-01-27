meta:
  id: dicom
  title: DICOM (Digital Imaging and Communications in Medicine)
  file-extension: dicom
  endian: le
  license: MIT
  encoding: UTF-8

doc: |
  DICOM (Digital Imaging and Communications in Medicine) is a standard for handling, storing, printing, and transmitting information in medical imaging.

seq:
  - id: preamble
    type: u4
    doc: Preamble (128 bytes, usually zero-filled)
    size: 128
  - id: prefix
    type: u4
    doc: Prefix (4 bytes, "DICM" in ASCII)
    valid: "DICM"
  - id: elements
    type: element
    repeat: eos
    doc: Sequence of DICOM elements

types:
  element:
    seq:
      - id: tag
        type: u2
        doc: Tag group (2 bytes)
      - id: tag_element
        type: u2
        doc: Tag element (2 bytes)
      - id: vr
        type: str
        size: 2
        doc: Value Representation (2 bytes, ASCII)
      - id: reserved
        type: u2
        if: vr == "OB" or vr == "OW" or vr == "SQ" or vr == "UN"
        doc: Reserved field (2 bytes, only for certain VRs)
      - id: length
        type: u2
        if: vr != "OB" and vr != "OW" and vr != "SQ" and vr != "UN"
        doc: Length of the value field (2 bytes)
      - id: length_long
        type: u4
        if: vr == "OB" or vr == "OW" or vr == "SQ" or vr == "UN"
        doc: Length of the value field (4 bytes, only for certain VRs)
      - id: value
        type: str
        size: length if vr != "OB" and vr != "OW" and vr != "SQ" and vr != "UN" else length_long
        doc: Value field (variable length)