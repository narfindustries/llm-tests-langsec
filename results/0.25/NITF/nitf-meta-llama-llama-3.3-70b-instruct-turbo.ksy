meta:
  id: nitf
  endian: be

seq:
  - id: file_header
    type: file_header
  - id: file_header_extension
    type: file_header_extension
  - id: file_security
    type: file_security
  - id: file_information
    type: file_information
  - id: file_length
    type: file_length
  - id: image_segment
    type: image_segment
    size: file_length.file_length
  - id: graphics_segment
    type: graphics_segment
    size: file_length.file_length
  - id: text_segment
    type: text_segment
    size: file_length.file_length
  - id: file_trailer
    type: file_trailer

types:
  file_header:
    seq:
      - id: fhdr
        type: str
        size: 9
        encoding: ascii
      - id: fver
        type: str
        size: 6
        encoding: ascii
      - id: ftype
        type: str
        size: 2
        encoding: ascii
      - id: fleve
        type: str
        size: 2
        encoding: ascii
      - id: fmodel
        type: str
        size: 2
        encoding: ascii
      - id: fcal
        type: str
        size: 1
        encoding: ascii

  file_header_extension:
    seq:
      - id: fhext
        type: str
        size: 3
        encoding: ascii
      - id: fhver
        type: str
        size: 2
        encoding: ascii
      - id: fhtype
        type: str
        size: 2
        encoding: ascii
      - id: fhlev
        type: str
        size: 2
        encoding: ascii
      - id: fhmodel
        type: str
        size: 2
        encoding: ascii
      - id: fhcal
        type: str
        size: 1
        encoding: ascii

  file_security:
    seq:
      - id: fsec
        type: str
        size: 1
        encoding: ascii
      - id: fscc
        type: str
        size: 3
        encoding: ascii
      - id: fsd
        type: str
        size: 8
        encoding: ascii
      - id: fscl
        type: str
        size: 1
        encoding: ascii
      - id: fsct
        type: str
        size: 2
        encoding: ascii
      - id: reserved
        type: str
        size: 65
        encoding: ascii

  file_information:
    seq:
      - id: fdatim
        type: str
        size: 14
        encoding: ascii
      - id: ftitle
        type: str
        size: 80
        encoding: ascii

  file_length:
    seq:
      - id: fl
        type: u4
        be: true

  image_segment:
    seq:
      - id: ihdr
        type: str
        size: 2
        encoding: ascii
      - id: iver
        type: str
        size: 6
        encoding: ascii
      - id: itype
        type: str
        size: 4
        encoding: ascii
      - id: ilev
        type: str
        size: 2
        encoding: ascii
      - id: imodel
        type: str
        size: 2
        encoding: ascii
      - id: ical
        type: str
        size: 1
        encoding: ascii
      - id: idat
        type: image_data

  graphics_segment:
    seq:
      - id: ghdr
        type: str
        size: 2
        encoding: ascii
      - id: gver
        type: str
        size: 6
        encoding: ascii
      - id: gtype
        type: str
        size: 6
        encoding: ascii
      - id: gleve
        type: str
        size: 2
        encoding: ascii
      - id: gmodel
        type: str
        size: 2
        encoding: ascii
      - id: gcal
        type: str
        size: 1
        encoding: ascii
      - id: gdat
        type: graphics_data

  text_segment:
    seq:
      - id: thdr
        type: str
        size: 2
        encoding: ascii
      - id: tver
        type: str
        size: 6
        encoding: ascii
      - id: ttype
        type: str
        size: 5
        encoding: ascii
      - id: tlev
        type: str
        size: 2
        encoding: ascii
      - id: tmodel
        type: str
        size: 2
        encoding: ascii
      - id: tcal
        type: str
        size: 1
        encoding: ascii
      - id: tdat
        type: text_data

  file_trailer:
    seq:
      - id: ft
        type: str
        size: 3
        encoding: ascii

  image_data:
    seq:
      - id: icode
        type: str
        size: 2
        encoding: ascii
      - id: idata
        type:
          switch-on: icode
          cases:
            NC: bytes
            JP: bytes
            Other: bytes

  graphics_data:
    seq:
      - id: gcode
        type: str
        size: 2
        encoding: ascii
      - id: gdata
        type:
          switch-on: gcode
          cases:
            NC: bytes
            JP: bytes
            Other: bytes

  text_data:
    seq:
      - id: tcode
        type: str
        size: 2
        encoding: ascii
      - id: tdata
        type:
          switch-on: tcode
          cases:
            NC: bytes
            JP: bytes
            Other: bytes