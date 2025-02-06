meta:
  id: dicom
  file-extension: dcm
  endian: le

seq:
  - id: preamble
    size: 128
  - id: magic
    contents: "DICM"
  - id: elements
    type: element
    repeat: eos

types:
  element:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2
      - id: vr
        size: 2
        type: str
        if: group >= 0x0002
      - id: reserved
        type: u2
        if: vr == 'OB' || vr == 'OW' || vr == 'OF' || vr == 'SQ' || vr == 'UT' || vr == 'UN'
      - id: value_length
        type: u4
        if: vr == 'OB' || vr == 'OW' || vr == 'OF' || vr == 'SQ' || vr == 'UT' || vr == 'UN'
      - id: value_length_short
        type: u2
        if: vr != 'OB' && vr != 'OW' && vr != 'OF' && vr != 'SQ' && vr != 'UT' && vr != 'UN'
      - id: value
        size: 
          if: vr == 'OB' || vr == 'OW' || vr == 'OF' || vr == 'SQ' || vr == 'UT' || vr == 'UN'
          value: value_length
        size: 
          if: vr != 'OB' && vr != 'OW' && vr != 'OF' && vr != 'SQ' && vr != 'UT' && vr != 'UN'
          value: value_length_short
        type:
          switch-on: vr
          cases:
            'AE': str
            'AS': str
            'CS': str
            'DA': str
            'DS': str
            'DT': str
            'IS': str
            'LO': str
            'LT': str
            'PN': str
            'SH': str
            'ST': str
            'TM': str
            'UI': str
            'UT': str
            'US': u2
            'UL': u4
            'SS': s2
            'SL': s4
            'FL': f4
            'FD': f8
            'SQ': seq_items

  seq_items:
    seq:
      - id: item_length
        type: u4
      - id: items
        type: element
        repeat: eos