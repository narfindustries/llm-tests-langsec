meta:
  id: nitf
  title: National Imagery Transmission Format (NITF)
  file-extension: nitf
  endian: be

seq:
  - id: file_header
    type: file_header

  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: file_header.numi

  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: file_header.numt

  - id: data_extension_segments
    type: data_extension_segment
    repeat: expr
    repeat-expr: file_header.numdes

  - id: reserved_extension_segments
    type: reserved_extension_segment
    repeat: expr
    repeat-expr: file_header.numres

types:
  file_header:
    seq:
      - id: fhdr
        type: str
        size: 4
        encoding: UTF-8

      - id: fver
        type: str
        size: 5
        encoding: UTF-8

      - id: clevel
        type: str
        size: 2
        encoding: UTF-8

      - id: stype
        type: str
        size: 4
        encoding: UTF-8

      - id: ostaid
        type: str
        size: 10
        encoding: UTF-8

      - id: fdt
        type: str
        size: 14
        encoding: UTF-8

      - id: fsclas
        type: str
        size: 1
        encoding: UTF-8

      - id: fsclsy
        type: str
        size: 2
        encoding: UTF-8

      - id: fscode
        type: str
        size: 11
        encoding: UTF-8

      - id: fsctlh
        type: str
        size: 2
        encoding: UTF-8

      - id: fsrel
        type: str
        size: 20
        encoding: UTF-8

      - id: fsdctp
        type: str
        size: 2
        encoding: UTF-8

      - id: fsdcdt
        type: str
        size: 8
        encoding: UTF-8

      - id: fsdcxm
        type: str
        size: 4
        encoding: UTF-8

      - id: fsdg
        type: str
        size: 1
        encoding: UTF-8

      - id: fsdgdt
        type: str
        size: 8
        encoding: UTF-8

      - id: fscltx
        type: str
        size: 43
        encoding: UTF-8

      - id: fscatp
        type: str
        size: 1
        encoding: UTF-8

      - id: fl
        type: str
        size: 12
        encoding: UTF-8

      - id: hl
        type: str
        size: 6
        encoding: UTF-8

      - id: numi
        type: u4

      - id: li
        type: u4
        repeat: expr
        repeat-expr: numi

      - id: numt
        type: u4

      - id: lt
        type: u4
        repeat: expr
        repeat-expr: numt

      - id: numdes
        type: u4

      - id: ldes
        type: u4
        repeat: expr
        repeat-expr: numdes

      - id: numres
        type: u4

      - id: lres
        type: u4
        repeat: expr
        repeat-expr: numres

  image_segment:
    seq:
      - id: iid1
        type: str
        size: 10
        encoding: UTF-8

      - id: idatim
        type: str
        size: 14
        encoding: UTF-8

      - id: tgtid
        type: str
        size: 17
        encoding: UTF-8

      - id: iid2
        type: str
        size: 80
        encoding: UTF-8

      - id: isorce
        type: str
        size: 42
        encoding: UTF-8

      - id: icat
        type: str
        size: 8
        encoding: UTF-8

      - id: imag
        type: str
        size: 4
        encoding: UTF-8

      - id: igeolo
        type: str
        size: 60
        encoding: UTF-8

      - id: ic
        type: str
        size: 2
        encoding: UTF-8

  text_segment:
    seq:
      - id: textid
        type: str
        size: 7
        encoding: UTF-8

      - id: txtalvl
        type: str
        size: 3
        encoding: UTF-8

      - id: txtfmt
        type: str
        size: 3
        encoding: UTF-8

      - id: txtitl
        type: str
        size: 80
        encoding: UTF-8

  data_extension_segment:
    seq:
      - id: desid
        type: str
        size: 25
        encoding: UTF-8

      - id: desver
        type: str
        size: 2
        encoding: UTF-8

      - id: declas
        type: str
        size: 1
        encoding: UTF-8

  reserved_extension_segment:
    seq:
      - id: resid
        type: str
        size: 25
        encoding: UTF-8

      - id: resver
        type: str
        size: 2
        encoding: UTF-8

      - id: reclas
        type: str
        size: 1
        encoding: UTF-8