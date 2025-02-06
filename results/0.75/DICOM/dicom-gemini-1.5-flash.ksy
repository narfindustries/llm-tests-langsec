types:
  dicom_attribute:
    seq:
      - id: tag
        type: u2be
      - id: vr
        type: str
        length: 2
        encoding: ASCII
      - id: length
        type: u4be
      - id: value
        type:
          switch-on: vr
          cases:
            AE:
              type: str
              encoding: ASCII
            AS:
              type: str
              encoding: ASCII
            AT:
              type: u4be
            CS:
              type: str
              encoding: ASCII
            DA:
              type: str
              encoding: ASCII
            DS:
              type: f4be
            DT:
              type: str
              encoding: ASCII
            FD:
              type: f8be
            FL:
              type: f4be
            IS:
              type: s4be
            LO:
              type: str
              encoding: ASCII
            LT:
              type: str
              encoding: ASCII
            OB:
              type: bytes
            OD:
              type: bytes
            OF:
              type: bytes
            OW:
              type: bytes
            PN:
              type: str
              encoding: ASCII
            SH:
              type: str
              encoding: ASCII
            SL:
              type: s4be
            SQ:
              type: seq
              seq_type: dicom_sequence
            SS:
              type: s2be
            ST:
              type: str
              encoding: ASCII
            TM:
              type: str
              encoding: ASCII
            UI:
              type: str
              encoding: ASCII
            UL:
              type: u4be
            UN:
              type: bytes
            US:
              type: u2be
            UT:
              type: str
              encoding: ASCII

  dicom_sequence:
    seq:
      - id: items
        type: seq
        seq_type: dicom_attribute


  dicom_file:
    seq:
      - id: preamble
        type: bytes
        length: 128
      - id: prefix
        type: str
        length: 4
        encoding: ASCII
      - id: dicom_elements
        type: seq
        seq_type: dicom_attribute

