meta:
  id: dicom
  file-extension: dcm
  endian: le
  license: CC0-1.0
  encoding: UTF-8
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
      - id: value_length
        type: value_length
        if: vr != "SQ"
      - id: value
        size: value_length
        if: vr != "SQ"
      - id: sequence
        type: sequence
        if: vr == "SQ"
  tag:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2
  vr:
    type: str
    size: 2
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
      SQ: Sequence
      SS: Signed Short
      ST: Short Text
      TM: Time
      UI: Unique Identifier
      UL: Unsigned Long
      UN: Unknown
      UR: URI/URL
      US: Unsigned Short
      UT: Unlimited Text
  value_length:
    seq:
      - id: length
        type: u2
        if: vr not in ["OB", "OD", "OF", "OL", "OW", "SQ", "UN"]
      - id: extended_length
        type: u4
        if: vr in ["OB", "OD", "OF", "OL", "OW", "SQ", "UN"]
  sequence:
    seq:
      - id: items
        type: item
        repeat: eos
  item:
    seq:
      - id: item_length
        type: u4
      - id: item_data
        size: item_length