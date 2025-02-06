meta:
  id: nitf
  file-extension: ntf
  endian: be
seq:
  - id: header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: header.numi.to_i
  - id: graphic_segments
    type: graphic_segment
    repeat: expr
    repeat-expr: header.nums.to_i
  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: header.numt.to_i
  - id: data_extension_segments
    type: des_segment
    repeat: expr
    repeat-expr: header.numdes.to_i
  - id: reserved_extension_segments
    type: res_segment
    repeat: expr
    repeat-expr: header.numres.to_i

types:
  des_segment:
    seq:
      - id: des_data
        size: 1

  res_segment:
    seq:
      - id: res_data
        size: 1

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
      - id: fsclsy
        type: str
        size: 2
        encoding: ASCII
      - id: fscode
        type: str
        size: 11
        encoding: ASCII
      - id: fsctlh
        type: str
        size: 2
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
        type: u1
      - id: fbkgc
        size: 3
      - id: oname
        type: str
        size: 24
        encoding: ASCII
      - id: ophone
        type: str
        size: 18
        encoding: ASCII
      - id: fl
        type: str
        size: 12
        encoding: ASCII
      - id: hl
        type: str
        size: 6
        encoding: ASCII
      - id: numi
        type: str
        size: 3
        encoding: ASCII
      - id: nums
        type: str
        size: 3
        encoding: ASCII
      - id: numt
        type: str
        size: 3
        encoding: ASCII
      - id: numdes
        type: str
        size: 3
        encoding: ASCII
      - id: numres
        type: str
        size: 3
        encoding: ASCII
      - id: udhdl
        type: str
        size: 5
        encoding: ASCII
      - id: udhofl
        type: str
        size: 3
        encoding: ASCII
        if: udhdl.to_i > 0
      - id: udhd
        size: udhdl.to_i - 3
        if: udhdl.to_i > 0
      - id: xhdl
        type: str
        size: 5
        encoding: ASCII
      - id: xhdlofl
        type: str
        size: 3
        encoding: ASCII
        if: xhdl.to_i > 0
      - id: xhd
        size: xhdl.to_i - 3
        if: xhdl.to_i > 0

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
      - id: isclsy
        type: str
        size: 2
        encoding: ASCII
      - id: iscode
        type: str
        size: 11
        encoding: ASCII
      - id: isctlh
        type: str
        size: 2
        encoding: ASCII
      - id: isrel
        type: str
        size: 20
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
        type: u1
      - id: isorce
        type: str
        size: 42
        encoding: ASCII
      - id: nrows
        type: str
        size: 8
        encoding: ASCII
      - id: ncols
        type: str
        size: 8
        encoding: ASCII
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
        type: str
        size: 2
        encoding: ASCII
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
        if: icords != ' '
      - id: nicom
        type: str
        size: 1
        encoding: ASCII
      - id: ic
        type: str
        size: 2
        encoding: ASCII
      - id: comrat
        type: str
        size: 4
        encoding: ASCII
        if: ic != 'NC'
      - id: nbands
        type: str
        size: 1
        encoding: ASCII
      - id: xbands
        type: str
        size: 5
        encoding: ASCII
        if: nbands == '0'
      - id: image_bands
        type: image_band
        repeat: expr
        repeat-expr: nbands.to_i
      - id: isync
        type: str
        size: 1
        encoding: ASCII
      - id: imode
        type: str
        size: 1
        encoding: ASCII
      - id: nbpr
        type: str
        size: 4
        encoding: ASCII
      - id: nbpc
        type: str
        size: 4
        encoding: ASCII
      - id: nppbh
        type: str
        size: 4
        encoding: ASCII
      - id: nppbv
        type: str
        size: 4
        encoding: ASCII
      - id: nbpp
        type: str
        size: 2
        encoding: ASCII
      - id: idlvl
        type: str
        size: 3
        encoding: ASCII
      - id: ialvl
        type: str
        size: 3
        encoding: ASCII
      - id: iloc
        type: str
        size: 10
        encoding: ASCII
      - id: imag
        type: str
        size: 4
        encoding: ASCII
      - id: udidl
        type: str
        size: 5
        encoding: ASCII
      - id: udofl
        type: str
        size: 3
        encoding: ASCII
        if: udidl.to_i > 0
      - id: udid
        size: udidl.to_i - 3
        if: udidl.to_i > 0
      - id: ixshdl
        type: str
        size: 5
        encoding: ASCII
      - id: ixsofl
        type: str
        size: 3
        encoding: ASCII
        if: ixshdl.to_i > 0
      - id: ixshd
        size: ixshdl.to_i - 3
        if: ixshdl.to_i > 0

  image_band:
    seq:
      - id: irepband
        type: str
        size: 2
        encoding: ASCII
      - id: isubcat
        type: str
        size: 6
        encoding: ASCII
      - id: ifc
        type: str
        size: 1
        encoding: ASCII
      - id: imflt
        type: str
        size: 3
        encoding: ASCII
      - id: nluts
        type: str
        size: 1
        encoding: ASCII
      - id: nelut
        type: str
        size: 5
        encoding: ASCII
        if: nluts != '0'

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
      - id: ssclsy
        type: str
        size: 2
        encoding: ASCII
      - id: sscode
        type: str
        size: 11
        encoding: ASCII
      - id: ssctlh
        type: str
        size: 2
        encoding: ASCII
      - id: ssrel
        type: str
        size: 20
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
      - id: ssctln
        type: str
        size: 15
        encoding: ASCII
      - id: encryp
        type: u1
      - id: sfmt
        type: str
        size: 1
        encoding: ASCII
      - id: sstruct
        type: str
        size: 13
        encoding: ASCII
      - id: sdlvl
        type: str
        size: 3
        encoding: ASCII
      - id: salvl
        type: str
        size: 3
        encoding: ASCII
      - id: sloc
        type: str
        size: 10
        encoding: ASCII
      - id: sbnd1
        type: str
        size: 10
        encoding: ASCII
      - id: scolor
        type: str
        size: 1
        encoding: ASCII
      - id: sbnd2
        type: str
        size: 10
        encoding: ASCII
      - id: sres2
        type: str
        size: 20
        encoding: ASCII
      - id: sxshdl
        type: str
        size: 5
        encoding: ASCII
      - id: sxsofl
        type: str
        size: 3
        encoding: ASCII
        if: sxshdl.to_i > 0
      - id: sxshd
        size: sxshdl.to_i - 3
        if: sxshdl.to_i > 0

  text_segment:
    seq:
      - id: te
        type: str
        size: 2
        encoding: ASCII
      - id: textid
        type: str
        size: 7
        encoding: ASCII
      - id: txtdt
        type: str
        size: 14
        encoding: ASCII
      - id: txtitl
        type: str
        size: 80
        encoding: ASCII
      - id: tsclas
        type: str
        size: 1
        encoding: ASCII
      - id: tsclsy
        type: str
        size: 2
        encoding: ASCII
      - id: tscode
        type: str
        size: 11
        encoding: ASCII
      - id: tsctlh
        type: str
        size: 2
        encoding: ASCII
      - id: tsrel
        type: str
        size: 20
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