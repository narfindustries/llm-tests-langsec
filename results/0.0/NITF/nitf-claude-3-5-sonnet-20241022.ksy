meta:
  id: nitf
  file-extension: ntf
  endian: be
seq:
  - id: file_header
    type: file_header
  - id: image_segments
    type: image_segment
    repeat: expr
    repeat-expr: file_header.numi.to_i
  - id: graphic_segments
    type: graphic_segment
    repeat: expr
    repeat-expr: file_header.nums.to_i
  - id: text_segments
    type: text_segment
    repeat: expr
    repeat-expr: file_header.numt.to_i
  - id: data_extension_segments
    type: data_extension_segment
    repeat: expr
    repeat-expr: file_header.numdes.to_i
  - id: reserved_extension_segments
    type: reserved_extension_segment
    repeat: expr
    repeat-expr: file_header.numres.to_i

types:
  text_segment:
    seq:
      - id: text_subheader
        type: text_subheader
      - id: text_data
        size: text_subheader.len.to_i

  text_subheader:
    seq:
      - id: te
        type: str
        size: 2
        encoding: ASCII
      - id: textid
        type: str
        size: 7
        encoding: ASCII
      - id: txtalvl
        type: str
        size: 3
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
      - id: encryp
        type: str
        size: 1
        encoding: ASCII
      - id: txtfmt
        type: str
        size: 3
        encoding: ASCII
      - id: len
        type: str
        size: 5
        encoding: ASCII

  data_extension_segment:
    seq:
      - id: des_subheader
        type: des_subheader
      - id: des_data
        size: des_subheader.len.to_i

  des_subheader:
    seq:
      - id: de
        type: str
        size: 2
        encoding: ASCII
      - id: desid
        type: str
        size: 25
        encoding: ASCII
      - id: len
        type: str
        size: 5
        encoding: ASCII

  reserved_extension_segment:
    seq:
      - id: res_subheader
        type: res_subheader
      - id: res_data
        size: res_subheader.len.to_i

  res_subheader:
    seq:
      - id: re
        type: str
        size: 2
        encoding: ASCII
      - id: resid
        type: str
        size: 25
        encoding: ASCII
      - id: len
        type: str
        size: 5
        encoding: ASCII

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
        type: str
        size: 1
        encoding: ASCII
      - id: fbkgc
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
      - id: lish
        type: str
        size: 6
        repeat: expr
        repeat-expr: numi.to_i
      - id: li
        type: str
        size: 10
        repeat: expr
        repeat-expr: numi.to_i
      - id: nums
        type: str
        size: 3
        encoding: ASCII
      - id: lssh
        type: str
        size: 4
        repeat: expr
        repeat-expr: nums.to_i
      - id: ls
        type: str
        size: 6
        repeat: expr
        repeat-expr: nums.to_i
      - id: numt
        type: str
        size: 3
        encoding: ASCII
      - id: ltsh
        type: str
        size: 4
        repeat: expr
        repeat-expr: numt.to_i
      - id: lt
        type: str
        size: 5
        repeat: expr
        repeat-expr: numt.to_i
      - id: numdes
        type: str
        size: 3
        encoding: ASCII
      - id: ldsh
        type: str
        size: 4
        repeat: expr
        repeat-expr: numdes.to_i
      - id: ld
        type: str
        size: 9
        repeat: expr
        repeat-expr: numdes.to_i
      - id: numres
        type: str
        size: 3
        encoding: ASCII
      - id: lresh
        type: str
        size: 4
        repeat: expr
        repeat-expr: numres.to_i
      - id: lre
        type: str
        size: 7
        repeat: expr
        repeat-expr: numres.to_i
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
        type: image_subheader
      - id: image_data
        size: im.nppbh.to_i * im.nbpr.to_i

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
        type: str
        size: 1
        encoding: ASCII
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
      - id: icom
        type: str
        size: 80
        encoding: ASCII
        repeat: expr
        repeat-expr: nicom.to_i
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
      - id: irepband
        type: str
        size: 2
        encoding: ASCII
        repeat: expr
        repeat-expr: nbands == '0' ? xbands.to_i : nbands.to_i
      - id: isubcat
        type: str
        size: 6
        encoding: ASCII
        repeat: expr
        repeat-expr: nbands == '0' ? xbands.to_i : nbands.to_i
      - id: ifc
        type: str
        size: 1
        encoding: ASCII
        repeat: expr
        repeat-expr: nbands == '0' ? xbands.to_i : nbands.to_i
      - id: imflt
        type: str
        size: 3
        encoding: ASCII
        repeat: expr
        repeat-expr: nbands == '0' ? xbands.to_i : nbands.to_i
      - id: nluts
        type: str
        size: 1
        encoding: ASCII
        repeat: expr
        repeat-expr: nbands == '0' ? xbands.to_i : nbands.to_i
      - id: nelut
        type: str
        size: 5
        encoding: ASCII
        repeat: expr
        repeat-expr: nluts.to_i
      - id: lutd
        size: 1
        repeat: expr
        repeat-expr: nelut.to_i
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

  graphic_segment:
    seq:
      - id: graphic_subheader
        type: graphic_subheader
      - id: graphic_data
        size: graphic_subheader.len.to_i

  graphic_subheader:
    seq:
      - id: sy
        type: str
        size: 2
        encoding: ASCII