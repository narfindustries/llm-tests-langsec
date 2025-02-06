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
      - id: fbkgd
        type: str
        size: 3
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
        type: u2
      - id: numi
        type: u2
      - id: li
        type: u4
        repeat: expr
        repeat-expr: numi
      - id: numsi
        type: u2
      - id: lsi
        type: u4
        repeat: expr
        repeat-expr: numsi
      - id: numt
        type: u2
      - id: lt
        type: u4
        repeat: expr
        repeat-expr: numt
      - id: numdes
        type: u2
      - id: ldes
        type: u4
        repeat: expr
        repeat-expr: numdes
      - id: numres
        type: u2
      - id: lres
        type: u4
        repeat: expr
        repeat-expr: numres
      - id: udhdl
        type: u4
      - id: udhofl
        type: u2
      - id: udhd
        size: udhdl
      - id: xhdl
        type: u4
      - id: xhdlof
        type: u2
      - id: xhd
        size: xhdl

  image_segment:
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
        size: 4
        encoding: ASCII
      - id: isdg
        type: str
        size: 1
        encoding: ASCII
      - id: isdgdt
        type: str
        size: 8
        encoding: ASCII
      - id: iscltx
        type: str
        size: 43
        encoding: ASCII
      - id: iscatp
        type: str
        size: 1
        encoding: ASCII
      - id: iscaut
        type: str
        size: 40
        encoding: ASCII
      - id: iscrsn
        type: str
        size: 1
        encoding: ASCII
      - id: issrdt
        type: str
        size: 8
        encoding: ASCII
      - id: isctln
        type: str
        size: 15
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
        type: u1
      - id: icom
        type: str
        size: 80
        encoding: ASCII
        repeat: expr
        repeat-expr: nicom

  graphic_segment:
    seq:
      - id: sy
        type: str
        size: 2
        encoding: ASCII
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
      - id: sscode
        type: str
        size: 40
        encoding: ASCII
      - id: ssctlh
        type: str
        size: 40
        encoding: ASCII
      - id: ssrel
        type: str
        size: 40
        encoding: ASCII
      - id: ssdctp
        type: str
        size: 2
        encoding: ASCII
      - id: ssdcdt
        type: str
        size: 8
        encoding: ASCII
      - id: ssdcxm
        type: str
        size: 4
        encoding: ASCII
      - id: ssdg
        type: str
        size: 1
        encoding: ASCII
      - id: ssdgdt
        type: str
        size: 8
        encoding: ASCII
      - id: sscltx
        type: str
        size: 43
        encoding: ASCII
      - id: sscatp
        type: str
        size: 1
        encoding: ASCII
      - id: sscaut
        type: str
        size: 40
        encoding: ASCII
      - id: sscrsn
        type: str
        size: 1
        encoding: ASCII
      - id: sssrdt
        type: str
        size: 8
        encoding: ASCII
      - id: ssctln
        type: str
        size: 15
        encoding: ASCII

  text_segment:
    seq:
      - id: te
        type: str
        size: 2
        encoding: ASCII
      - id: tid
        type: str
        size: 10
        encoding: ASCII
      - id: ttitle
        type: str
        size: 80
        encoding: ASCII
      - id: tsclas
        type: str
        size: 1
        encoding: ASCII
      - id: tscode
        type: str
        size: 40
        encoding: ASCII
      - id: tsctlh
        type: str
        size: 40
        encoding: ASCII
      - id: tsrel
        type: str
        size: 40
        encoding: ASCII
      - id: tsdctp
        type: str
        size: 2
        encoding: ASCII
      - id: tsdcdt
        type: str
        size: 8
        encoding: ASCII
      - id: tsdcxm
        type: str
        size: 4
        encoding: ASCII
      - id: tsdg
        type: str
        size: 1
        encoding: ASCII
      - id: tsdgdt
        type: str
        size: 8
        encoding: ASCII
      - id: tscltx
        type: str
        size: 43
        encoding: ASCII
      - id: tscatp
        type: str
        size: 1
        encoding: ASCII
      - id: tscaut
        type: str
        size: 40
        encoding: ASCII
      - id: tscrsn
        type: str
        size: 1
        encoding: ASCII
      - id: tssrdt
        type: str
        size: 8
        encoding: ASCII
      - id: tsctln
        type: str
        size: 15
        encoding: ASCII

  data_extension_segment:
    seq:
      - id: de
        type: str
        size: 2
        encoding: ASCII
      - id: dedata
        size-eos: true

  reserved_extension_segment:
    seq:
      - id: re
        type: str
        size: 2
        encoding: ASCII
      - id: redata
        size-eos: true