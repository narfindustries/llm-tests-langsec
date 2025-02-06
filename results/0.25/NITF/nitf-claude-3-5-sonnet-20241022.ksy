meta:
  id: nitf
  file-extension: ntf
  endian: be

seq:
  - id: file_header
    type: file_header_type
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
  file_header_type:
    seq:
      - id: fhdr
        type: str
        size: 4
        encoding: ascii
      - id: fver
        type: str
        size: 5
        encoding: ascii
      - id: clevel
        type: str
        size: 2
        encoding: ascii
      - id: stype
        type: str
        size: 4
        encoding: ascii
      - id: ostaid
        type: str
        size: 10
        encoding: ascii
      - id: fdt
        type: str
        size: 14
        encoding: ascii
      - id: ftitle
        type: str
        size: 80
        encoding: ascii
      - id: fsclas
        type: str
        size: 1
        encoding: ascii
      - id: fsclsy
        type: str
        size: 2
        encoding: ascii
      - id: fscode
        type: str
        size: 11
        encoding: ascii
      - id: fsctlh
        type: str
        size: 2
        encoding: ascii
      - id: fsrel
        type: str
        size: 20
        encoding: ascii
      - id: fsdctp
        type: str
        size: 2
        encoding: ascii
      - id: fsdcdt
        type: str
        size: 8
        encoding: ascii
      - id: fsdcxm
        type: str
        size: 4
        encoding: ascii
      - id: fsdg
        type: str
        size: 1
        encoding: ascii
      - id: fsdgdt
        type: str
        size: 8
        encoding: ascii
      - id: fscltx
        type: str
        size: 43
        encoding: ascii
      - id: fscatp
        type: str
        size: 1
        encoding: ascii
      - id: fscaut
        type: str
        size: 40
        encoding: ascii
      - id: fscrsn
        type: str
        size: 1
        encoding: ascii
      - id: fssrdt
        type: str
        size: 8
        encoding: ascii
      - id: fsctln
        type: str
        size: 15
        encoding: ascii
      - id: encryp
        type: str
        size: 1
        encoding: ascii
      - id: fbkgc
        type: str
        size: 3
        encoding: ascii
      - id: oname
        type: str
        size: 24
        encoding: ascii
      - id: ophone
        type: str
        size: 18
        encoding: ascii
      - id: fl
        type: str
        size: 12
        encoding: ascii
      - id: hl
        type: str
        size: 6
        encoding: ascii
      - id: numi
        type: str
        size: 3
        encoding: ascii
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
        encoding: ascii
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
        encoding: ascii
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
        encoding: ascii
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
        encoding: ascii
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
        encoding: ascii
      - id: udhofl
        type: str
        size: 3
        encoding: ascii
        if: udhdl.to_i > 0
      - id: udhd
        size: udhdl.to_i - 3
        if: udhdl.to_i > 0
      - id: xhdl
        type: str
        size: 5
        encoding: ascii
      - id: xhdlofl
        type: str
        size: 3
        encoding: ascii
        if: xhdl.to_i > 0
      - id: xhd
        size: xhdl.to_i - 3
        if: xhdl.to_i > 0

  image_segment:
    seq:
      - id: im
        type: image_subheader
      - id: image_data
        size: im.length.to_i

  image_subheader:
    seq:
      - id: im
        type: str
        size: 2
        encoding: ascii
      - id: iid1
        type: str
        size: 10
        encoding: ascii
      - id: idatim
        type: str
        size: 14
        encoding: ascii
      - id: tgtid
        type: str
        size: 17
        encoding: ascii
      - id: iid2
        type: str
        size: 80
        encoding: ascii
      - id: isclas
        type: str
        size: 1
        encoding: ascii
      - id: isclsy
        type: str
        size: 2
        encoding: ascii
      - id: iscode
        type: str
        size: 11
        encoding: ascii
      - id: isctlh
        type: str
        size: 2
        encoding: ascii
      - id: isrel
        type: str
        size: 20
        encoding: ascii
      - id: isdctp
        type: str
        size: 2
        encoding: ascii
      - id: isdcdt
        type: str
        size: 8
        encoding: ascii
      - id: isdcxm
        type: str
        size: 4
        encoding: ascii
      - id: isdg
        type: str
        size: 1
        encoding: ascii
      - id: isdgdt
        type: str
        size: 8
        encoding: ascii
      - id: iscltx
        type: str
        size: 43
        encoding: ascii
      - id: iscatp
        type: str
        size: 1
        encoding: ascii
      - id: iscaut
        type: str
        size: 40
        encoding: ascii
      - id: iscrsn
        type: str
        size: 1
        encoding: ascii
      - id: issrdt
        type: str
        size: 8
        encoding: ascii
      - id: isctln
        type: str
        size: 15
        encoding: ascii
      - id: encryp
        type: str
        size: 1
        encoding: ascii
      - id: isorce
        type: str
        size: 42
        encoding: ascii
      - id: nrows
        type: str
        size: 8
        encoding: ascii
      - id: ncols
        type: str
        size: 8
        encoding: ascii
      - id: pvtype
        type: str
        size: 3
        encoding: ascii
      - id: irep
        type: str
        size: 8
        encoding: ascii
      - id: icat
        type: str
        size: 8
        encoding: ascii
      - id: abpp
        type: str
        size: 2
        encoding: ascii
      - id: pjust
        type: str
        size: 1
        encoding: ascii
      - id: icords
        type: str
        size: 1
        encoding: ascii
      - id: igeolo
        type: str
        size: 60
        encoding: ascii
        if: icords.str != " "
      - id: nicom
        type: str
        size: 1
        encoding: ascii
      - id: icom
        type: str
        size: 80
        encoding: ascii
        repeat: expr
        repeat-expr: nicom.to_i
      - id: ic
        type: str
        size: 2
        encoding: ascii
      - id: comrat
        type: str
        size: 4
        encoding: ascii
        if: ic.str != "NC"
      - id: nbands
        type: str
        size: 1
        encoding: ascii
      - id: xbands
        type: str
        size: 5
        encoding: ascii
        if: nbands.str == "0"
      - id: irepband
        type: str
        size: 2
        encoding: ascii
        repeat: expr
        repeat-expr: nbands.str == "0" ? xbands.to_i : nbands.to_i
      - id: isubcat
        type: str
        size: 6
        encoding: ascii
        repeat: expr
        repeat-expr: nbands.str == "0" ? xbands.to_i : nbands.to_i
      - id: ifc
        type: str
        size: 1
        encoding: ascii
        repeat: expr
        repeat-expr: nbands.str == "0" ? xbands.to_i : nbands.to_i
      - id: imflt
        type: str
        size: 3
        encoding: ascii
        repeat: expr
        repeat-expr: nbands.str == "0" ? xbands.to_i : nbands.to_i
      - id: nluts
        type: str
        size: 1
        encoding: ascii
        repeat: expr
        repeat-expr: nbands.str == "0" ? xbands.to_i : nbands.to_i
      - id: nelut
        type: str
        size: 5
        encoding: ascii
        repeat: expr
        repeat-expr: nbands.str == "0" ? xbands.to_i : nbands.to_i
      - id: length
        type: str
        size: 10
        encoding: ascii

  graphic_segment:
    seq:
      - id: graphic_subheader
        type: graphic_subheader_type
      - id: graphic_data
        size: graphic_subheader.length.to_i

  graphic_subheader_type:
    seq:
      - id: sy
        type: str
        size: 2
        encoding: ascii
      - id: sid
        type: str
        size: 10
        encoding: ascii
      - id: sname
        type: str
        size: 20
        encoding: ascii
      - id: ssclas
        type: str
        size: 1
        encoding: ascii
      - id: encryp
        type: str
        size: 1
        encoding: ascii
      - id: sfmt
        type: str
        size: 1
        encoding: ascii
      - id: sstruct
        type: str
        size: 13
        encoding: ascii
      - id: sdlvl
        type: str
        size: 3
        encoding: ascii
      - id: salvl
        type: str
        size: 3
        encoding: ascii
      - id: sloc
        type: str
        size: 10
        encoding: ascii
      - id: sbnd1
        type: str
        size: 10
        encoding: ascii
      - id: scolor
        type: str
        size: 1
        encoding: ascii
      - id: sbnd2
        type: str
        size: 10
        encoding: ascii
      - id: sres2
        type: str
        size: 2
        encoding: ascii
      - id: length
        type: str
        size: 10
        encoding: ascii

  text_segment:
    seq:
      - id: text_subheader
        type: text_subheader_type
      - id: text_data
        size: text_subheader.length.to_i

  text_subheader_type:
    seq:
      - id: te
        type: str
        size: 2
        encoding: ascii
      - id: textid
        type: str
        size: 7
        encoding: ascii
      - id: txtalvl
        type: str
        size: 3
        encoding: ascii
      - id: txtdt
        type: str
        size: 14
        encoding: ascii
      - id: txtitl
        type: str
        size: 80
        encoding: ascii
      - id: tsclas
        type: str
        size: 1
        encoding: ascii
      - id: encryp
        type: str
        size: 1
        encoding: ascii
      - id: txtfmt
        type: str
        size: 3
        encoding: ascii
      - id: length
        type: str
        size: 5
        encoding: ascii

  data_extension_segment:
    seq:
      - id: des_subheader
        type: des_subheader_type
      - id: des_data
        size: des_subheader.length.to_i

  des_subheader_type:
    seq:
      - id: de
        type: str
        size: 2
        encoding: ascii
      - id: desid
        type: str
        size: 25
        encoding: ascii
      - id: desver
        type: str
        size: 2
        encoding: ascii
      - id: desclas
        type: str
        size: 1
        encoding: ascii
      - id: encryp
        type: str
        size: 1
        encoding: ascii
      - id: length
        type: str
        size: 9
        encoding: ascii

  reserved_extension_segment:
    seq:
      - id: res_subheader
        type: res_subheader_type
      - id: res_data
        size: res_subheader.length.to_i

  res_subheader_type:
    seq:
      - id: re
        type: str
        size: 2
        encoding: ascii
      - id: resid
        type: str
        size: 25
        encoding: ascii
      - id: resver
        type: str
        size: 2
        encoding: ascii
      - id: resclas
        type: str
        size: 1
        encoding: ascii
      - id: encryp
        type: str
        size: 1
        encoding: ascii
      - id: length
        type: str
        size: 7
        encoding: ascii