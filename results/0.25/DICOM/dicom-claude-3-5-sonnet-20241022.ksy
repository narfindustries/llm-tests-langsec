meta:
  id: dicom
  file-extension: dcm
  endian: le
  encoding: ascii

seq:
  - id: preamble
    size: 128
  - id: dicom_prefix
    contents: 'DICM'
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
        size: 2
        encoding: ascii
        if: not is_implicit_vr
      - id: reserved
        type: u2
        if: is_explicit_vr_and_long
      - id: length
        type:
          switch-on: is_implicit_vr
          cases:
            true: u4
            false: length_field
      - id: data
        size: data_size
    instances:
      is_implicit_vr:
        value: _root.transfer_syntax == "1.2.840.10008.1.2"
      is_explicit_vr_and_long:
        value: >-
          not is_implicit_vr and
          (vr == "OB" or vr == "OW" or vr == "OF" or vr == "SQ" or
           vr == "UT" or vr == "UN")
      data_size:
        value: >-
          length.value == 0xffffffff ? 0 : length.value
      tag:
        value: (tag_group << 16) | tag_element

  length_field:
    seq:
      - id: value
        type:
          switch-on: _parent.is_explicit_vr_and_long
          cases:
            true: u4
            false: u2

  sequence_item:
    seq:
      - id: length
        type: u4
      - id: elements
        type: element
        repeat: eos
        if: length != 0

  sequence:
    seq:
      - id: items
        type: sequence_item
        repeat: until
        repeat-until: _.length == 0

enums:
  vr_type:
    16705: application_entity  # AE
    16723: age_string         # AS
    16724: attribute_tag      # AT
    17235: code_string        # CS
    17473: date              # DA
    17491: decimal_string    # DS
    17492: date_time        # DT
    17988: float_double     # FD
    18252: float            # FL
    18771: integer_string   # IS
    19535: long_string      # LO
    19540: long_text        # LT
    20290: other_byte       # OB
    20292: other_double     # OD
    20294: other_float      # OF
    20300: other_long       # OL
    20311: other_word       # OW
    20558: person_name      # PN
    21320: short_string     # SH
    21356: signed_long      # SL
    21361: sequence_items   # SQ
    21363: signed_short     # SS
    21364: short_text       # ST
    21581: time            # TM
    21827: unlimited_chars  # UC
    21833: unique_identifier # UI
    21836: unsigned_long    # UL
    21838: unknown          # UN
    21842: universal_resource # UR
    21843: unsigned_short   # US
    21844: unlimited_text   # UT