meta:
  id: dicom
  title: DICOM
  file-extension: dcm
  endian: le

seq:
  - id: preamble
    size: 128
  - id: prefix
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
        if: is_extended_length
      - id: value_length
        type: u4
        if: is_extended_length
      - id: value_length_short
        type: u2
        if: not is_extended_length
      - id: value
        size: value_length if is_extended_length else value_length_short
        type: bytes

  is_extended_length:
    doc: Checks if the VR requires an extended length
    type: b1
    instances:
      value:
        value: vr in ["OB", "OW", "OF", "SQ", "UT", "UN"]

enums:
  vr_enum:
    AE: "AE"
    AS: "AS"
    AT: "AT"
    DA: "DA"
    DS: "DS"
    DT: "DT"
    FD: "FD"
    FL: "FL"
    IS: "IS"
    LO: "LO"
    LT: "LT"
    OB: "OB"
    OF: "OF"
    OW: "OW"
    PN: "PN"
    SH: "SH"
    SL: "SL"
    SQ: "SQ"
    SS: "SS"
    ST: "ST"
    TM: "TM"
    UI: "UI"
    UL: "UL"
    US: "US"
    UT: "UT"
    UN: "UN"