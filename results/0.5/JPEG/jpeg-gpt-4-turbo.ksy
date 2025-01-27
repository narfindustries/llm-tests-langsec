meta:
  id: jpeg
  file-extension: jpg
  endian: be
  title: JPEG (Joint Photographic Experts Group) image
  license: CC0-1.0
  encoding: ASCII

doc: |
  JPEG is a commonly used method of lossy compression for digital images, particularly for those images produced by digital photography. JPEG's compression algorithm is at its best on photographs and paintings of realistic scenes with smooth variations of tone and color.

seq:
  - id: segments
    type: segment
    repeat: eos

types:
  segment:
    seq:
      - id: magic
        contents: [0xff]
      - id: marker
        enum: marker_enum
        type: u1
      - id: length
        type: u2
        if: marker != marker_enum::soi and marker != marker_enum::eoi
      - id: data
        size: length - 2
        if: length > 2

enums:
  marker_enum:
    0xd8: soi
    0xc0: sof0
    0xc2: sof2
    0xc4: dht
    0xdb: dqt
    0xdd: dri
    0xda: sos
    0xfe: com
    0xd9: eoi
    0xe0: app0
    0xe1: app1
    0xe2: app2
    0xe3: app3
    0xe4: app4
    0xe5: app5
    0xe6: app6
    0xe7: app7
    0xe8: app8
    0xe9: app9
    0xea: appa
    0xeb: appb
    0xec: appc
    0xed: appd
    0xee: appe
    0xef: appf
    0xf0: jpg0
    0xf1: jpg1
    0xf2: jpg2
    0xf3: jpg3
    0xf4: jpg4
    0xf5: jpg5
    0xf6: jpg6
    0xf7: jpg7
    0xf8: jpg8
    0xf9: jpg9
    0xfa: jpga
    0xfb: jpgb
    0xfc: jpgc
    0xfd: jpgd