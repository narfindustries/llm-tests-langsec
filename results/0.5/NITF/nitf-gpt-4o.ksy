meta:
  id: nitf
  title: National Imagery Transmission Format (NITF)
  file-extension: nitf
  endian: be
  encoding: ASCII

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
      - id: fscctlh
        type: str
        size: 40
        encoding: ASCII
      - id: fsrel
        type: str
        size: 40
        encoding: ASCII
      - id: fscaut
        type: str
        size: 20
        encoding: ASCII
      - id: fsctln
        type: str
        size: 20
        encoding: ASCII
      - id: fsdwng
        type: str
        size: 6
        encoding: ASCII
      - id: fsdwngdt
        type: str
        size: 8
        encoding: ASCII
      - id: fsdevt
        type: str
        size: 40
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
        type: u4
      - id: hl
        type: u4
      - id: numi
        type: u2
      - id: lish
        type: u4
      - id: li
        type: u4
        repeat: expr
        repeat-expr: numi
      - id: nums
        type: u2
      - id: lssh
        type: u4
      - id: ls
        type: u4
        repeat: expr
        repeat-expr: nums
      - id: numt
        type: u2
      - id: ltsh
        type: u4
      - id: lt
        type: u4
        repeat: expr
        repeat-expr: numt
      - id: numdes
        type: u2
      - id: ldesh
        type: u4
      - id: lde
        type: u4
        repeat: expr
        repeat-expr: numdes
      - id: numres
        type: u2
      - id: lresh
        type: u4
      - id: lre
        type: u4
        repeat: expr
        repeat-expr: numres

  image_subheader:
    seq:
      - id: im
        type: str
        size: 2
        encoding: ASCII
      - id: iid1
        type: str
        size: 10
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
      - id: iscauth
        type: str
        size: 20
        encoding: ASCII
      - id: isctln
        type: str
        size: 20
        encoding: ASCII
      - id: isdwng
        type: str
        size: 6
        encoding: ASCII
      - id: isdwngdt
        type: str
        size: 8
        encoding: ASCII
      - id: isdevt
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
      - id: nicom
        type: u2
      - id: icom
        type: str
        repeat: expr
        repeat-expr: nicom
        size: 80
        encoding: ASCII

  text_subheader:
    seq:
      - id: te
        type: str
        size: 2
        encoding: ASCII
      - id: textid
        type: str
        size: 80
        encoding: ASCII
      - id: txtalvl
        type: str
        size: 1
        encoding: ASCII
      - id: txtcode
        type: str
        size: 40
        encoding: ASCII
      - id: txtctlh
        type: str
        size: 40
        encoding: ASCII
      - id: txtrel
        type: str
        size: 40
        encoding: ASCII
      - id: txtauth
        type: str
        size: 20
        encoding: ASCII
      - id: txtctln
        type: str
        size: 20
        encoding: ASCII
      - id: txtdwng
        type: str
        size: 6
        encoding: ASCII
      - id: txtdwngdt
        type: str
        size: 8
        encoding: ASCII
      - id: txtdevt
        type: str
        size: 40
        encoding: ASCII

  data_extension_subheader:
    seq:
      - id: de
        type: str
        size: 2
        encoding: ASCII
      - id: desid
        type: str
        size: 25
        encoding: ASCII
      - id: desver
        type: u2
      - id: declas
        type: str
        size: 1
        encoding: ASCII
      - id: decode
        type: str
        size: 40
        encoding: ASCII
      - id: dectlh
        type: str
        size: 40
        encoding: ASCII
      - id: derel
        type: str
        size: 40
        encoding: ASCII
      - id: decaut
        type: str
        size: 20
        encoding: ASCII
      - id: dectln
        type: str
        size: 20
        encoding: ASCII
      - id: dedwng
        type: str
        size: 6
        encoding: ASCII
      - id: dedwngdt
        type: str
        size: 8
        encoding: ASCII
      - id: dedevt
        type: str
        size: 40
        encoding: ASCII