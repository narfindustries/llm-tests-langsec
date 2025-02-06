meta:
  id: tiff
  endian: le

seq:
  - id: header
    type: tiff_header

types:
  tiff_header:
    seq:
      - id: byte_order
        type: str
        size: 2
        encoding: ascii
      - id: version
        type: uint16
      - id: ifd_offset
        type: uint32

  ifd:
    seq:
      - id: num_tags
        type: uint16
      - id: tags
        type: tiff_tag
        repeat: num_tags
      - id: next_ifd_offset
        type: uint32

  tiff_tag:
    seq:
      - id: tag_id
        type: uint16
      - id: tag_type
        type: uint16
      - id: num_values
        type: uint32
      - id: value_offset
        type: uint32
        if: num_values > 4 or (tag_type in [1, 2, 7] and num_values > 1)
      - id: value
        type:
          switch-on: tag_id
          cases:
            254: uint32
            255: uint32
            256: uint32
            257: uint32
            258: uint16
            259: uint16
            262: uint16
            263: uint16
            264: uint16
            265: uint16
            266: uint16
            269: str
            270: str
            271: str
            272: str
            273: uint32
            274: uint16
            277: uint16
            278: uint32
            279: uint32
            280: uint16
            281: uint16
            282: rational
            283: rational
            284: uint16
            285: str
            286: rational
            287: rational
            288: uint32
            289: uint32
            290: uint16
            291: uint16
            292: uint32
            293: uint32
            296: uint16
            297: uint16
            301: uint16
            305: str
            306: str
            315: str
            316: str
            317: uint16
            318: rational
            319: rational
            320: uint16
            321: uint16
            322: uint32
            323: uint32
            324: uint32
            325: uint32
            332: uint16
            333: str
            334: uint16
            336: uint16
            337: str
            338: uint16
            339: uint16
            340: 
              switch-on: tag_type
              cases:
                1: uint8
                3: uint16
                4: uint32
            341: 
              switch-on: tag_type
              cases:
                1: uint8
                3: uint16
                4: uint32
            342: uint32
            343: uint8
            344: uint32
            345: uint32
            346: uint16
            347: bytes
            351: bytes
            400: ifd
            401: uint32
            403: uint32
            404: uint32
            405: uint32
            406: uint32
            410: uint16
            434: uint32

  rational:
    seq:
      - id: numerator
        type: uint32
      - id: denominator
        type: uint32