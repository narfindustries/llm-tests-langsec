meta:
  id: dicom
  endian: le
  file-extension: dcm
  title: DICOM
  application:
    - Digital Imaging and Communications in Medicine
  xref:
    wikidata: Q211173
  license: CC0-1.0

seq:
  - id: preamble
    size: 128
  - id: magic
    contents: "DICM"
  - id: elements
    type: dicom_element
    repeat: eos

types:
  dicom_element:
    seq:
      - id: tag_group
        type: u2
      - id: tag_element
        type: u2
      - id: vr
        type: str
        size: 2
      - id: reserved
        size: 2
        if: vr == "OB" || vr == "OD" || vr == "OF" || vr == "OL" || vr == "OW" || vr == "SQ" || vr == "UC" || vr == "UR" || vr == "UT" || vr == "UN"
      - id: value_length
        type: u4
        if: vr == "OB" || vr == "OD" || vr == "OF" || vr == "OL" || vr == "OW" || vr == "SQ" || vr == "UC" || vr == "UR" || vr == "UT" || vr == "UN"
      - id: value_length_short
        type: u2
        if: not (vr == "OB" || vr == "OD" || vr == "OF" || vr == "OL" || vr == "OW" || vr == "SQ" || vr == "UC" || vr == "UR" || vr == "UT" || vr == "UN")
      - id: value
        size: value_length if vr == "OB" || vr == "OD" || vr == "OF" || vr == "OL" || vr == "OW" || vr == "SQ" || vr == "UC" || vr == "UR" || vr == "UT" || vr == "UN"
        size: value_length_short if not (vr == "OB" || vr == "OD" || vr == "OF" || vr == "OL" || vr == "OW" || vr == "SQ" || vr == "UC" || vr == "UR" || vr == "UT" || vr == "UN")