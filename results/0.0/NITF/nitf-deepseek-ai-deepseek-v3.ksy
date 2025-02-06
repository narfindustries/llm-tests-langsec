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
        encoding: ASCII
        size: 4
      - id: fver
        type: str
        encoding: ASCII
        size: 5
      - id: clevel
        type: str
        encoding: ASCII
        size: 2
      - id: stype
        type: str
        encoding: ASCII
        size: 4
      - id: ostaid
        type: str
        encoding: ASCII
        size: 10
      - id: fdt
        type: str
        encoding: ASCII
        size: 14
      - id: ftitle
        type: str
        encoding: ASCII
        size: 80
      - id: fsclas
        type: str
        encoding: ASCII
        size: 1
      - id: fscode
        type: str
        encoding: ASCII
        size: 40
      - id: fsctlh
        type: str
        encoding: ASCII
        size: 40
      - id: fsrel
        type: str
        encoding: ASCII
        size: 40
      - id: fsdctp
        type: str
        encoding: ASCII
        size: 2
      - id: fsdcdt
        type: str
        encoding: ASCII
        size: 8
      - id: fsdcxm
        type: str
        encoding: ASCII
        size: 20
      - id: fsdg
        type: str
        encoding: ASCII
        size: 20
      - id: fsdgdt
        type: str
        encoding: ASCII
        size: 8
      - id: fscltx
        type: str
        encoding: ASCII
        size: 40
      - id: fscop
        type: str
        encoding: ASCII
        size: 5
      - id: fscpys
        type: str
        encoding: ASCII
        size: 5
      - id: oname
        type: str
        encoding: ASCII
        size: 24
      - id: ophone
        type: str
        encoding: ASCII
        size: 18
  image_segment_header:
    seq:
      - id: im
        type: str
        encoding: ASCII
        size: 2
      - id: iid
        type: str
        encoding: ASCII
        size: 10
      - id: idatim
        type: str
        encoding: ASCII
        size: 14
      - id: tgtid
        type: str
        encoding: ASCII
        size: 17
      - id: isclas
        type: str
        encoding: ASCII
        size: 1
      - id: iscode
        type: str
        encoding: ASCII
        size: 40
      - id: isctlh
        type: str
        encoding: ASCII
        size: 40
      - id: isrel
        type: str
        encoding: ASCII
        size: 40
      - id: isdctp
        type: str
        encoding: ASCII
        size: 2
      - id: isdcdt
        type: str
        encoding: ASCII
        size: 8
      - id: isdcxm
        type: str
        encoding: ASCII
        size: 20
      - id: isdg
        type: str
        encoding: ASCII
        size: 20
      - id: isdgdt
        type: str
        encoding: ASCII
        size: 8
      - id: iscltx
        type: str
        encoding: ASCII
        size: 40
      - id: encrypt
        type: str
        encoding: ASCII
        size: 1
      - id: isorce
        type: str
        encoding: ASCII
        size: 42
      - id: nrows
        type: str
        encoding: ASCII
        size: 8
      - id: ncols
        type: str
        encoding: ASCII
        size: 8
      - id: pvtype
        type: str
        encoding: ASCII
        size: 3
      - id: irep
        type: str
        encoding: ASCII
        size: 8
      - id: icat
        type: str
        encoding: ASCII
        size: 8
      - id: abpp
        type: str
        encoding: ASCII
        size: 2
      - id: pjust
        type: str
        encoding: ASCII
        size: 1
      - id: icords
        type: str
        encoding: ASCII
        size: 1
  graphic_segment_header:
    seq:
      - id: sy
        type: str
        encoding: ASCII
        size: 2
      - id: sid
        type: str
        encoding: ASCII
        size: 10
      - id: sdatim
        type: str
        encoding: ASCII
        size: 14
      - id: ssclas
        type: str
        encoding: ASCII
        size: 1
      - id: sscode
        type: str
        encoding: ASCII
        size: 40
      - id: ssctlh
        type: str
        encoding: ASCII
        size: 40
      - id: ssrel
        type: str
        encoding: ASCII
        size: 40
      - id: ssdctp
        type: str
        encoding: ASCII
        size: 2
      - id: ssdcdt
        type: str
        encoding: ASCII
        size: 8
      - id: ssdcxm
        type: str
        encoding: ASCII
        size: 20
      - id: ssdg
        type: str
        encoding: ASCII
        size: 20
      - id: ssdgdt
        type: str
        encoding: ASCII
        size: 8
      - id: sscltx
        type: str
        encoding: ASCII
        size: 40
  text_segment_header:
    seq:
      - id: te
        type: str
        encoding: ASCII
        size: 2
      - id: textid
        type: str
        encoding: ASCII
        size: 7
      - id: txtalvl
        type: str
        encoding: ASCII
        size: 3
      - id: txtdt
        type: str
        encoding: ASCII
        size: 14
      - id: txtitl
        type: str
        encoding: ASCII
        size: 80
      - id: txsclas
        type: str
        encoding: ASCII
        size: 1
      - id: txscode
        type: str
        encoding: ASCII
        size: 40
      - id: txsctlh
        type: str
        encoding: ASCII
        size: 40
      - id: txsrel
        type: str
        encoding: ASCII
        size: 40
      - id: txsdctp
        type: str
        encoding: ASCII
        size: 2
      - id: txsdcdt
        type: str
        encoding: ASCII
        size: 8
      - id: txsdcxm
        type: str
        encoding: ASCII
        size: 20
      - id: txsdg
        type: str
        encoding: ASCII
        size: 20
      - id: txsdgdt
        type: str
        encoding: ASCII
        size: 8
      - id: txscltx
        type: str
        encoding: ASCII
        size: 40
  data_extension_segment_header:
    seq:
      - id: de
        type: str
        encoding: ASCII
        size: 2
      - id: desid
        type: str
        encoding: ASCII
        size: 25
      - id: desver
        type: str
        encoding: ASCII
        size: 2
      - id: desclas
        type: str
        encoding: ASCII
        size: 1
      - id: descode
        type: str
        encoding: ASCII
        size: 40
      - id: desctlh
        type: str
        encoding: ASCII
        size: 40
      - id: desrel
        type: str
        encoding: ASCII
        size: 40
      - id: desdctp
        type: str
        encoding: ASCII
        size: 2
      - id: desdcdt
        type: str
        encoding: ASCII
        size: 8
      - id: desdcxm
        type: str
        encoding: ASCII
        size: 20
      - id: desdg
        type: str
        encoding: ASCII
        size: 20
      - id: desdgdt
        type: str
        encoding: ASCII
        size: 8
      - id: descltx
        type: str
        encoding: ASCII
        size: 40