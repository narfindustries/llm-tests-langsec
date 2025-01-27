meta:
  id: dicom
  title: DICOM
  application: Digital Imaging and Communications in Medicine
  file_extension: dcm
  endian: le
  ks-version: 0.9

doc: |
  DICOM is a standard for handling, storing, printing, and transmitting information
  in medical imaging. It includes a file format definition and a network communications
  protocol. This specification covers the basic file format structure.

seq:
  - id: preamble
    size: 128
  - id: magic
    type: str
    size: 4
    doc: |
      Magic bytes `DICM` which indicate the start of the DICOM file meta information.
    assert: _ == "DICM"
  - id: file_meta_information
    type: file_meta_information

types:
  file_meta_information:
    seq:
      - id: group_length
        type: u4
        doc: "Length of the file meta information group."
      - id: dataset_elements
        type: dataset_element
        repeat: eos
        doc: "Sequence of dataset elements."

  dataset_element:
    seq:
      - id: tag_group
        type: u2
      - id: tag_element
        type: u2
      - id: vr
        type: str
        size: 2
        doc: "Value representation. Indicates the data type."
      - id: reserved
        type: u2
        if: vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN"
      - id: value_length
        type: u2
        if: vr != "OB" and vr != "OW" and vr != "OF" and vr != "SQ" and vr != "UT" and vr != "UN"
      - id: value_length_extended
        type: u4
        if: vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN"
      - id: value
        size: value_length
        if: vr != "OB" and vr != "OW" and vr != "OF" and vr != "SQ" and vr != "UT" and vr != "UN"
      - id: value_extended
        size: value_length_extended
        if: vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN"