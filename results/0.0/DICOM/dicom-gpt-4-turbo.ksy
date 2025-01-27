meta:
  id: dicom
  file-extension: dcm
  endian: le
  title: "Digital Imaging and Communications in Medicine (DICOM)"
  license: CC0-1.0
  ks-version: 0.9

doc: |
  DICOM is the standard for the communication and management of medical imaging information and related data. It is most commonly used for storing, transmitting, or viewing medical images, such as MRIs, CT scans, and ultrasound images.

seq:
  - id: preamble
    size: 128
    doc: "The DICOM file preamble, which is a fixed 128-byte field. It is typically set to all zeroes."
  - id: prefix
    size: 4
    contents: 'DICM'
    doc: "DICOM prefix that follows the preamble to indicate a valid DICOM file."
  - id: elements
    type: data_element
    repeat: eos
    doc: "Sequence of data elements that contain the actual data."

types:
  data_element:
    seq:
      - id: tag_group
        type: u2
      - id: tag_element
        type: u2
      - id: vr
        type: str
        size: 2
        doc: "Value Representation (VR) that defines the data type and format of the attribute value."
      - id: reserved
        type: u2
        if: "vr in ['OB', 'OW', 'OF', 'SQ', 'UT', 'UN']"
        doc: "Reserved bytes, used only for certain VRs."
      - id: value_length
        type: u4
        doc: "The length of the value field. A value of 0xFFFFFFFF indicates that the value field is of undefined length."
      - id: value_field
        size: value_length
        if: value_length != 0xFFFFFFFF
        doc: "The data for the element, the format and content of which are determined by the VR."

enums:
  vr_type:
    OB: "Other Byte"
    OW: "Other Word"
    OF: "Other Float"
    SQ: "Sequence of Items"
    UT: "Unlimited Text"
    UN: "Unknown"