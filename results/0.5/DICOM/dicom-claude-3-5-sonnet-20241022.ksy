meta:
  id: dicom
  file-extension: dcm
  endian: le

seq:
  - id: preamble
    size: 128
  - id: dicom_prefix
    contents: 'DICM'
  - id: meta_information
    type: meta_information_group
  - id: data_set
    type: data_elements
    repeat: eos

types:
  meta_information_group:
    seq:
      - id: group_tag
        contents: [0x02, 0x00]
      - id: meta_elements
        type: data_elements
        repeat: until
        repeat-until: _io.pos >= meta_length + meta_length_pos
    instances:
      meta_length_pos:
        value: _io.pos
      meta_length:
        value: '_root.data_set[0].vl'

  data_elements:
    seq:
      - id: tag_group
        type: u2
      - id: tag_element
        type: u2
      - id: vr
        type: str
        size: 2
        encoding: ASCII
        if: not is_implicit_vr
      - id: reserved
        type: u2
        if: is_explicit_vr_with_length
      - id: vl
        type: u4
        if: is_explicit_vr_with_length
      - id: vl_short
        type: u2
        if: is_explicit_vr_without_length
      - id: vl_implicit
        type: u4
        if: is_implicit_vr
      - id: value
        size: value_length
    instances:
      is_implicit_vr:
        value: '_root.meta_information.meta_elements[0].value == 1'
      is_explicit_vr_with_length:
        value: '(not is_implicit_vr) and (vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or vr == "UT" or vr == "UN")'
      is_explicit_vr_without_length:
        value: '(not is_implicit_vr) and (not is_explicit_vr_with_length)'
      value_length:
        value: |
          is_explicit_vr_with_length ? vl :
          is_explicit_vr_without_length ? vl_short :
          vl_implicit

  sequence_item:
    seq:
      - id: length
        type: u4
      - id: data
        type: data_elements
        repeat: eos
        if: length != 0xffffffff

  pixel_data:
    seq:
      - id: pixel_data
        size-eos: true

enums:
  transfer_syntaxes:
    1: implicit_vr_little_endian
    2: explicit_vr_little_endian
    3: explicit_vr_big_endian
    4: jpeg_baseline
    5: jpeg_extended
    6: jpeg_lossless
    7: jpeg_2000_lossless
    8: jpeg_2000
    9: rle_lossless

  vr_types:
    0x4145: application_entity  # AE
    0x4153: age_string         # AS
    0x4154: attribute_tag      # AT
    0x4353: code_string        # CS
    0x4441: date              # DA
    0x4453: decimal_string    # DS
    0x4454: date_time         # DT
    0x464C: floating_point_single  # FL
    0x4644: floating_point_double  # FD
    0x4953: integer_string    # IS
    0x4C4F: long_string       # LO
    0x4C54: long_text         # LT
    0x4F42: other_byte        # OB
    0x4F44: other_double      # OD
    0x4F46: other_float       # OF
    0x4F4C: other_long        # OL
    0x4F57: other_word        # OW
    0x504E: person_name       # PN
    0x5348: short_string      # SH
    0x534C: signed_long       # SL
    0x5351: sequence          # SQ
    0x5353: signed_short      # SS
    0x5354: short_text        # ST
    0x544D: time             # TM
    0x5543: unlimited_characters  # UC
    0x5549: unique_identifier # UI
    0x554C: unsigned_long     # UL
    0x554E: unknown           # UN
    0x5552: universal_resource # UR
    0x5553: unsigned_short    # US
    0x5554: unlimited_text    # UT