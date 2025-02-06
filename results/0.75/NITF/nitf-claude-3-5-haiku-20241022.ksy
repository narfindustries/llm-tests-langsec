meta:
  id: nitf
  file-extension: ntf
  endian: be
seq:
  - id: file_header
    type: file_header
types:
  file_header:
    seq:
      - id: fhdr
        type: str
        size: 9
        encoding: ASCII
        valid: '"NITF02.10"'
      - id: clevel
        type: str
        size: 2
        valid:
          any-of: ["01", "02", "03", "04", "05", "06", "07", "08", "09"]
      - id: stype
        type: str
        size: 1
        valid:
          any-of: ["U", "R", "C", "S", "T"]
      - id: ostaid
        type: str
        size: 10
      - id: fdt
        type: str
        size: 14
        pattern: '\d{14}'
      - id: ftitle
        type: str
        size: 80
      - id: fscop
        type: str
        size: 2
      - id: fscpyr
        type: str
        size: 2
      - id: fssn
        type: str
        size: 4
      - id: numdlvs
        type: str
        size: 3
        pattern: '\d{3}'
      - id: numhlvs
        type: str
        size: 3
        pattern: '\d{3}'
      - id: resrvd1
        type: str
        size: 3
      - id: originating_station
        type: str
        size: 10
      - id: security_group
        type: security_group
  
  security_group:
    seq:
      - id: security_classification
        type: str
        size: 1
      - id: security_country_code
        type: str
        size: 2
      - id: security_release_instructions
        type: str
        size: 2
      - id: security_description_length
        type: str
        size: 4
        pattern: '\d{4}'
      - id: security_description
        type: str
        size-eos: true
        if: security_description_length != '0000'