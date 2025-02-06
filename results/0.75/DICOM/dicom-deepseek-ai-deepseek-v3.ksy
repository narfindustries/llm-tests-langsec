meta:
  id: dicom
  title: DICOM
  license: CC0-1.0
  endian: le
seq:
  - id: preamble
    type: u4
    repeat: expr
    repeat-expr: 128
  - id: prefix
    contents: "DICM"
  - id: elements
    type: element
    repeat: eos
types:
  element:
    seq:
      - id: tag_group
        type: u2
      - id: tag_element
        type: u2
      - id: vr
        type: str
        encoding: ASCII
        size: 2
      - id: value_length
        type: u2
      - id: value
        size: value_length
        type:
          switch-on: vr
          cases:
            "AE": str_ae
            "AS": str_as
            "AT": u4
            "CS": str_cs
            "DA": str_da
            "DS": str_ds
            "DT": str_dt
            "FL": f4
            "FD": f8
            "IS": str_is
            "LO": str_lo
            "LT": str_lt
            "OB": blob
            "OD": blob
            "OF": blob
            "OL": blob
            "OW": blob
            "PN": str_pn
            "SH": str_sh
            "SL": s4
            "SQ": sequence
            "SS": s2
            "ST": str_st
            "TM": str_tm
            "UI": str_ui
            "UL": u4
            "UN": blob
            "US": u2
            "UT": str_ut
  str_ae:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  str_as:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  str_cs:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  str_da:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  str_ds:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  str_dt:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  str_is:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  str_lo:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  str_lt:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  blob:
    seq:
      - id: value
        type: str
        encoding: HEX
        size-eos: true
  str_pn:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  str_sh:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  str_st:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  str_tm:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  str_ui:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  str_ut:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  sequence:
    seq:
      - id: items
        type: sequence_item
        repeat: eos
  sequence_item:
    seq:
      - id: item_length
        type: u4
      - id: item_value
        size: item_length
        type: element