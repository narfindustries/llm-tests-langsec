meta:
  id: dicom
  file-extension: dcm
  endian: be

seq:
  - id: preamble
    size: 128
    type: bytes

  - id: prefix
    size: 4
    type: str
    encoding: ascii

  - id: transfer_syntax_uid
    size: 2
    type: str
    encoding: ascii

  - id: group_0002
    type: seq
    repeat: expr
    seq:
      - id: group_number
        size: 2
        type: uint16

      - id: element_number
        size: 2
        type: uint16

      - id: value_representation
        size: 2
        type: str
        encoding: ascii

      - id: reserved
        size: 2
        type: bytes

      - id: value_length
        size: 2
        type: uint16

      - id: value
        size: value_length
        type: switch
        cases:
          - {on: value_representation, equals: 'OB', type: bytes}
          - {on: value_representation, equals: 'UI', type: bytes}
          - {on: value_representation, equals: 'LO', type: str}
          - {on: value_representation, equals: 'PN', type: str}
          - {on: value_representation, equals: 'DT', type: str}
          - {on: value_representation, equals: 'TM', type: str}
          - {on: value_representation, equals: 'DA', type: str}
          - {on: value_representation, equals: 'AS', type: str}

  - id: group_0004
    type: seq
    repeat: expr
    seq:
      - id: group_number
        size: 2
        type: uint16

      - id: element_number
        size: 2
        type: uint16

      - id: length
        size: 4
        type: uint32

      - id: items
        type: seq
        repeat: expr
        seq:
          - id: item
            type: seq
            repeat: expr
            seq:
              - id: item_group_number
                size: 2
                type: uint16

              - id: item_element_number
                size: 2
                type: uint16

              - id: item_value_representation
                size: 2
                type: str
                encoding: ascii

              - id: item_reserved
                size: 2
                type: bytes

              - id: item_value_length
                size: 2
                type: uint16

              - id: item_value
                size: item_value_length
                type: switch
                cases:
                  - {on: item_value_representation, equals: 'OB', type: bytes}
                  - {on: item_value_representation, equals: 'UI', type: bytes}
                  - {on: item_value_representation, equals: 'LO', type: str}
                  - {on: item_value_representation, equals: 'PN', type: str}
                  - {on: item_value_representation, equals: 'DT', type: str}
                  - {on: item_value_representation, equals: 'TM', type: str}
                  - {on: item_value_representation, equals: 'DA', type: str}
                  - {on: item_value_representation, equals: 'AS', type: str}