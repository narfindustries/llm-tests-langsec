meta:
  id: dicom
  title: DICOM File Format
  file-extension: dcm
  endian: le

seq:
  - id: preamble
    size: 128
  - id: magic
    contents: "DICM"
  - id: elements
    type: data_element
    repeat: eos

types:
  data_element:
    seq:
      - id: tag_group
        type: u2
      - id: tag_element
        type: u2
      - id: vr
        type: str
        size: 2
        encoding: ASCII
      - id: reserved
        type: u2
        if: vr in ['OB', 'OW', 'OF', 'SQ', 'UT', 'UN']
      - id: value_length
        type: u4
        if: vr in ['OB', 'OW', 'OF', 'SQ', 'UT', 'UN']
      - id: value_length_short
        type: u2
        if: vr not in ['OB', 'OW', 'OF', 'SQ', 'UT', 'UN']
      - id: value
        size: 
          switch-on: vr
          cases:
            'OB': value_length
            'OW': value_length
            'OF': value_length
            'SQ': value_length
            'UT': value_length
            'UN': value_length
            _: value_length_short