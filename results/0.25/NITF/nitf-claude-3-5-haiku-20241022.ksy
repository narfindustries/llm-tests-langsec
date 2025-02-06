doc: National Imagery Transmission Format (NITF) 2.1 File Format
meta:
  id: nitf
  endian: be
  encoding: ASCII
seq:
  - id: file_header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: eos
types:
  file_header:
    seq:
      - id: fhdr
        contents: "NITF"
      - id: fver
        type: str
        size: 5
        encoding: ASCII
      - id: clevel
        type: u1
      - id: stype
        type: str
        size: 10
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
      - id: security
        type: security_block
      - id: fscop
        type: u2
      - id: fscpys
        type: u2
      - id: encryption
        type: encryption_block
      - id: background_color
        type: str
        size: 3
        encoding: ASCII
  security_block:
    seq:
      - id: classification
        type: str
        size: 1
        encoding: ASCII
      - id: country_code
        type: str
        size: 2
        encoding: ASCII
      - id: release_instructions
        type: str
        size: 20
        encoding: ASCII
  encryption_block:
    seq:
      - id: encryption_type
        type: u1
      - id: encryption_key
        type: str
        size: 32
        encoding: ASCII
  image_segment:
    seq:
      - id: image_header
        type: image_header
      - id: image_data
        size-eos: true
  image_header:
    seq:
      - id: itype
        type: str
        size: 3
        encoding: ASCII
      - id: irep
        type: str
        size: 8
        encoding: ASCII
      - id: icat
        type: str
        size: 3
        encoding: ASCII
      - id: nbands
        type: u2
      - id: imode
        type: str
        size: 1
        encoding: ASCII
      - id: nbpc
        type: u1
      - id: nppbh
        type: u2
      - id: nppbv
        type: u2
      - id: nbpp
        type: u1
      - id: idlvl
        type: u2
      - id: ialvl
        type: u2
      - id: iloc
        type: str
        size: 10
        encoding: ASCII
      - id: imag
        type: str
        size: 4
        encoding: ASCII
      - id: geolocation
        type: geolocation_block
  geolocation_block:
    seq:
      - id: coords
        type: coordinate
        repeat: expr
        repeat-expr: 4
  coordinate:
    seq:
      - id: lat
        type: f8
      - id: lon
        type: f8