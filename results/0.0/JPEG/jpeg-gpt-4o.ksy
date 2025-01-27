meta:
  id: jpeg
  title: JPEG File Interchange Format
  file-extension: jpg
  xref:
    mime: image/jpeg
  license: CC0-1.0
  endian: be

seq:
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.marker == 0xd9

types:
  segment:
    seq:
      - id: marker
        type: u2
      - id: length
        type: u2
        if: marker != 0xd8 and marker != 0xd9
      - id: data
        size: length - 2
        if: marker != 0xd8 and marker != 0xd9

enums:
  marker:
    0xffd8: soi
    0xffd9: eoi
    0xffe0: app0
    0xffe1: app1
    0xffe2: app2
    0xffe3: app3
    0xffe4: app4
    0xffe5: app5
    0xffe6: app6
    0xffe7: app7
    0xffe8: app8
    0xffe9: app9
    0xffea: app10
    0xffeb: app11
    0xffec: app12
    0xffed: app13
    0xffee: app14
    0xffef: app15
    0xffc0: sof0
    0xffc1: sof1
    0xffc2: sof2
    0xffc3: sof3
    0xffc5: sof5
    0xffc6: sof6
    0xffc7: sof7
    0xffc9: sof9
    0xffca: sof10
    0xffcb: sof11
    0xffcd: sof13
    0xffce: sof14
    0xffcf: sof15
    0xffc4: dht
    0xffdb: dqt
    0xffdd: dri
    0xffda: sos
    0xfffe: com
    0xff01: tem
    0xfff0: rst0
    0xfff1: rst1
    0xfff2: rst2
    0xfff3: rst3
    0xfff4: rst4
    0xfff5: rst5
    0xfff6: rst6
    0xfff7: rst7
    0xfff8: rst8
    0xfff9: rst9
    0xfffa: rst10
    0xfffb: rst11
    0xfffc: rst12
    0xfffd: rst13