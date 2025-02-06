meta:
  id: dicom
  title: DICOM (Digital Imaging and Communications in Medicine)
  license: CC0-1.0
  endian: le
  encoding: ASCII
seq:
  - id: preamble
    size: 128
  - id: prefix
    contents: "DICM"
  - id: elements
    type: element
    repeat: eos
types:
  element:
    seq:
      - id: tag
        type: tag
      - id: vr
        type: vr
      - id: length
        type: length
        if: vr.is_explicit
      - id: value
        size: length.value
        if: length.value != 0
  tag:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2
  vr:
    type: str
    size: 2
    enum: vr_type
  length:
    type: u2
    if: vr.is_short
    else:
      type: u4
      if: vr.is_long
  vr_type:
    enum:
      AE: Application Entity
      AS: Age String
      AT: Attribute Tag
      CS: Code String
      DA: Date
      DS: Decimal String
      DT: Date Time
      FL: Floating Point Single
      FD: Floating Point Double
      IS: Integer String
      LO: Long String
      LT: Long Text
      OB: Other Byte
      OD: Other Double
      OF: Other Float
      OL: Other Long
      OW: Other Word
      PN: Person Name
      SH: Short String
      SL: Signed Long
      SQ: Sequence of Items
      SS: Signed Short
      ST: Short Text
      TM: Time
      UI: Unique Identifier
      UL: Unsigned Long
      UN: Unknown
      US: Unsigned Short
      UT: Unlimited Text
  is_explicit:
    value: vr != "UN"
  is_short:
    value: vr in ["AE", "AS", "AT", "CS", "DA", "DS", "DT", "FL", "FD", "IS", "LO", "LT", "OB", "OD", "OF", "OL", "OW", "PN", "SH", "SL", "SQ", "SS", "ST", "TM", "UI", "UL", "UN", "US", "UT"]
  is_long:
    value: vr in ["OB", "OD", "OF", "OL", "OW", "SQ", "UN", "UT"]