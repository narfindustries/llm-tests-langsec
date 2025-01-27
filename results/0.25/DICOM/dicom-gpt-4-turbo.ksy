meta:
  id: dicom
  file-extension: dcm
  endian: le
  title: Digital Imaging and Communications in Medicine (DICOM)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  DICOM is the standard for the communication and management of medical imaging information and related data. It is most commonly used for storing, transmitting, and processing medical images such as ultrasounds, MRIs, and CT scans.

seq:
  - id: preamble
    size: 128
    doc: The DICOM format preamble; contains 128 bytes reserved for file meta information.

  - id: magic
    contents: [0x44, 0x49, 0x43, 0x4D]
    doc: DICOM magic number used to identify the file format.

  - id: file_meta_information_group_length
    type: data_element_explicit
    doc: File Meta Information Group Length.

  - id: file_meta_information
    type: file_meta_information
    size-eos: true
    doc: The File Meta Information section.

types:
  data_element_explicit:
    seq:
      - id: group_number
        type: u2
      - id: element_number
        type: u2
      - id: vr
        type: str
        size: 2
      - id: reserved
        type: u2
        if: vr in ["OB", "OW", "OF", "SQ", "UT", "UN"]
      - id: value_length
        type: u4
      - id: value_field
        size: value_length

  file_meta_information:
    seq:
      - id: elements
        type: data_element_explicit
        repeat: eos

  data_element_implicit:
    seq:
      - id: group_number
        type: u2
      - id: element_number
        type: u2
      - id: value_length
        type: u4
      - id: value_field
        size: value_length