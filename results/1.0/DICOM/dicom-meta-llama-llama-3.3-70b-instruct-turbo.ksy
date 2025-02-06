meta:
  id: dicom
  file-extension: dcm
  endian: le
  fields:
    - id: preamble
      size: 128
      type: bytes

    - id: prefix
      size: 4
      type: str
      encoding: ascii

    - id: dcm_header
      type: dcm_header

types:
  dcm_header:
    seq:
      - id: group_0002
        type: data_element
      - id: group_0004
        type: data_element
      - id: transfer_syntax_uid
        type: data_element
      - id: implicit_vr
        type: data_element
      - id: groups
        type: group_data_elements
        repeat: until eof

  group_data_elements:
    seq:
      - id: group_number
        type: u2
      - id: element_data
        type: data_element

  data_element:
    seq:
      - id: tag_group
        type: u2
      - id: tag_element
        type: u2
      - id: vr
        type: str
        size: 2
        encoding: ascii
      - id: length
        type: u2
      - id: value
        type:
          switch-on: vr
          cases:
            UL: u4
            US: u2
            SS: s2
            SL: s4
            FL: f4
            FD: f8
            UI: bytes
            OB: bytes
            OW: bytes
            SQ: seq
            UN: bytes
            SH: s2
            AE: bytes
            LO: bytes
            LT: bytes
            ST: bytes
            PN: bytes
            UC: bytes
            UR: bytes
            UT: bytes

  data_set:
    seq:
      - id: items
        type: data_set_item
        repeat: until eof

  data_set_item:
    seq:
      - id: data_element
        type: data_element_switch

  data_element_switch:
    switch-on: tag_group
    cases:
      8:
        switch-on: tag_element
        cases:
          5: data_element
          16: data_element
          18: data_element
          22: data_element
          24: data_element
          40: data_element
          50: data_element
          60: data_element
          64: data_element
          100: data_element
          102: data_element
          103: data_element
          104: data_element
          120: data_element
          150: data_element
          1000: data_element
          1001: data_element
          1002: data_element
          1005: data_element
          1020: data_element
          1030: data_element
          1040: data_element
          1100: data_element
          1200: data_element
      10:
        switch-on: tag_element
        cases:
          10: data_element
          20: data_element
          30: data_element
          40: data_element
          1000: data_element
          1001: data_element
          1005: data_element
          1010: data_element
          1020: data_element
          1030: data_element
          1040: data_element
          2200: data_element
          2210: data_element
      18:
        switch-on: tag_element
        cases:
          15: data_element
          22: data_element
          24: data_element
          1000: data_element
          1001: data_element
          1002: data_element
          1008: data_element
      20:
        switch-on: tag_element
        cases:
          13: data_element
          14: data_element
          15: data_element
          50: data_element
          60: data_element
          1000: data_element
          1001: data_element
          1002: data_element
          1010: data_element
          1050: data_element
      28:
        switch-on: tag_element
        cases:
          10: data_element
          11: data_element
          100: data_element
          101: data_element
          102: data_element
          103: data_element
          110: data_element
          114: data_element
          1050: data_element
          1051: data_element
      2048:
        switch-on: tag_element
        cases:
          10: bytes

instances:
  - name: dicom_data_set
    type: data_set
    start: 128