meta:
  id: nitf
  title: National Imagery Transmission Format (NITF)
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
        size: 9
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
        size: 40
        encoding: ASCII
      - id: fsctlh
        type: str
        size: 40
        encoding: ASCII
      - id: fsrel
        type: str
        size: 40
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
        size: 40
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
      - id: numi
        type: u2
      - id: numg
        type: u2
      - id: numt
        type: u2
      - id: numdes
        type: u2
      - id: numres
        type: u2

  image_segment:
    seq:
      - id: im
        type: str
        size: 10
        encoding: ASCII
      - id: iid1
        type: str
        size: 80
        encoding: ASCII
      - id: idatim
        type: str
        size: 14
        encoding: ASCII
      - id: tgtid
        type: str
        size: 17
        encoding: ASCII
      - id: iid2
        type: str
        size: 80
        encoding: ASCII
      - id: isclas
        type: str
        size: 1
        encoding: ASCII
      - id: iscode
        type: str
        size: 40
        encoding: ASCII
      - id: isctlh
        type: str
        size: 40
        encoding: ASCII
      - id: isrel
        type: str
        size: 40
        encoding: ASCII
      - id: isdctp
        type: str
        size: 2
        encoding: ASCII
      - id: isdcdt
        type: str
        size: 8
        encoding: ASCII
      - id: isdcxm
        type: str
        size: 40
        encoding: ASCII
      - id: encryp
        type: str
        size: 1
        encoding: ASCII
      - id: isorce
        type: str
        size: 42
        encoding: ASCII
      - id: nrows
        type: u4
      - id: ncols
        type: u4
      - id: pvtype
        type: str
        size: 3
        encoding: ASCII
      - id: irep
        type: str
        size: 8
        encoding: ASCII
      - id: icat
        type: str
        size: 8
        encoding: ASCII
      - id: abpp
        type: u2
      - id: pjust
        type: str
        size: 1
        encoding: ASCII
      - id: icords
        type: str
        size: 1
        encoding: ASCII
      - id: igeolo
        type: str
        size: 60
        encoding: ASCII

  graphic_segment:
    seq:
      - id: sid
        type: str
        size: 10
        encoding: ASCII
      - id: sname
        type: str
        size: 20
        encoding: ASCII
      - id: ssclas
        type: str
        size: 1
        encoding: ASCII
      - id: ssrel
        type: str
        size: 40
        encoding: ASCII
      - id: encryp
        type: str
        size: 1
        encoding: ASCII

  text_segment:
    seq:
      - id: textid
        type: str
        size: 10
        encoding: ASCII
      - id: txtalvl
        type: str
        size: 1
        encoding: ASCII
      - id: txtfmt
        type: str
        size: 3
        encoding: ASCII
      - id: encryp
        type: str
        size: 1
        encoding: ASCII

  data_extension_segment:
    seq:
      - id: desid
        type: str
        size: 25
        encoding: ASCII
      - id: desver
        type: str
        size: 2
        encoding: ASCII
      - id: declas
        type: str
        size: 1
        encoding: ASCII
      - id: encryp
        type: str
        size: 1
        encoding: ASCII

  reserved_extension_segment:
    seq:
      - id: resid
        type: str
        size: 25
        encoding: ASCII
      - id: resver
        type: str
        size: 2
        encoding: ASCII
      - id: reclas
        type: str
        size: 1
        encoding: ASCII
      - id: encryp
        type: str
        size: 1
        encoding: ASCII