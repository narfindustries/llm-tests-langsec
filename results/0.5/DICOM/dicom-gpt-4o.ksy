meta:
  id: dicom
  title: DICOM File Format
  file-extension: dcm
  endian: le
seq:
  - id: preamble
    size: 128
    doc: |
      The preamble is 128 bytes and should be ignored. It is usually filled with zeros.
  - id: magic
    contents: "DICM"
    doc: |
      The DICOM prefix is "DICM" and is used to confirm that the file is a DICOM file.
  - id: elements
    type: dicom_elements
    doc: |
      The main content of the DICOM file, consisting of a series of data elements.
types:
  dicom_elements:
    seq:
      - id: elements
        type: dicom_element
        repeat: eos
  dicom_element:
    seq:
      - id: tag_group
        type: u2
      - id: tag_element
        type: u2
      - id: vr
        type: str
        size: 2
        encoding: ASCII
        if: tag_group != 0x0002
        doc: |
          Value Representation (VR) is a two-byte code that describes the data type.
      - id: reserved
        size: 2
        if: tag_group != 0x0002 and (vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN")
        doc: |
          Reserved bytes should be zero for certain VRs.
      - id: value_length
        type: u4
        if: tag_group == 0x0002 or (vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN")
      - id: value_length_short
        type: u2
        if: tag_group != 0x0002 and not (vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN")
      - id: value
        size: value_length
        if: tag_group == 0x0002 or (vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN")
      - id: value_short
        size: value_length_short
        if: tag_group != 0x0002 and not (vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN")