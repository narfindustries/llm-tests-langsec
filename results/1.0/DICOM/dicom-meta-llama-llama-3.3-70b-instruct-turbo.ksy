meta:
  id: dicom
  file-extension: dcm
  endian: be
seq:
  - id: preamble
    size: 128
  - id: prefix
    size: 4
    type: str
    encoding: ascii
  - id: transfer_syntax_uid
    size: 2
    type: str
    encoding: ascii
  - id: ult
    type: ul
    size: 2
  - id: Wonderland
    type: seq
    repeat: expr
    seq:
      - id: tag_group
        type: ul
        size: 2
      - id: tag_element
        type: ul
        size: 2
      - id: vr
        type: str
        size: 2
        encoding: ascii
      - id: reserved
        size: 2
      - id: ult
        type: ul
        size: 2
      - id: Wonderland2
        type: switch
        cases:
          - when: tag_group != 0
            then: Wonderland2a
          - when: true
            then: Wonderland2b
        types:
          Wonderland2a:
            seq:
              - id: value
                size: ult
                type: str
                encoding: ascii
          Wonderland2b:
            seq:
              - id: item
                type: seq
                repeat: expr
                seq:
                  - id: tag_group_item
                    type: ul
                    size: 2
                  - id: tag_element_item
                    type: ul
                    size: 2
                  - id: vr_item
                    type: str
                    size: 2
                    encoding: ascii
                  - id: reserved_item
                    size: 2
                  - id: ult_item
                    type: ul
                    size: 2
                  - id: value_item
                    size: ult_item
                    type: str
                    encoding: ascii