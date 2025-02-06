meta:
  id: nitf
  file-extension: nitf
  endian: be
seq:
  - id: file_header
    type: file_header
types:
  file_header:
    seq:
      - id: fhdr
        type: str
        size: 4
        encoding: ASCII
      - id: fver
        type: str
        size: 5
        encoding: ASCII
      - id: clevel
        type: str
        size: 2
        encoding: ASCII
      - id: stype
        type: str
        size: 4
        encoding: ASCII
      - id: ostaid
        type: str
        size: 10
        encoding: ASCII
      - id: fdt
        type: str
        size: 14
        encoding: ASCII
      - id: ftitle
        type: str
        size: 80
        encoding: ASCII
      - id: fsclas
        type: str
        size: 1
        encoding: ASCII
      - id: fscode
        type: str
        size: 2
        encoding: ASCII
      - id: fsctlh
        type: str
        size: 20
        encoding: ASCII
      - id: fsrel
        type: str
        size: 20
        encoding: ASCII
      - id: fsdctp
        type: str
        size: 2
        encoding: ASCII
      - id: fsdcdt
        type: str
        size: 8
        encoding: ASCII
      - id: fsdcxm
        type: str
        size: 4
        encoding: ASCII
      - id: fsdg
        type: str
        size: 1
        encoding: ASCII
      - id: fsdgdt
        type: str
        size: 8
        encoding: ASCII
      - id: fscltx
        type: str
        size: 43
        encoding: ASCII
      - id: fscatp
        type: str
        size: 1
        encoding: ASCII
      - id: fscaut
        type: str
        size: 40
        encoding: ASCII
      - id: fscrsn
        type: str
        size: 1
        encoding: ASCII
      - id: fssrdt
        type: str
        size: 8
        encoding: ASCII
      - id: fsctln
        type: str
        size: 15
        encoding: ASCII
      - id: fscop
        type: str
        size: 5
        encoding: ASCII
      - id: fscpys
        type: str
        size: 5
        encoding: ASCII
      - id: encryp
        type: str
        size: 1
        encoding: ASCII
      - id: fbgkc
        type: str
        size: 6
        encoding: ASCII
      - id: oname
        type: str
        size: 24
        encoding: ASCII
      - id: ophone
        type: str
        size: 18
        encoding: ASCII
      - id: fl
        type: u8