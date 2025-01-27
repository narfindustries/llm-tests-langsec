meta:
  id: jpeg-gemini-1
  title: JPEG Gemini 1.5 Flash
  homepage: ""
  doc_url: ""
  license: ""

types:
  marker:
    enum:
      - 0xffd8: SOI
      - 0xffd9: EOI
      - 0xffda: SOS
      - 0xffdb: DQT
      - 0xffdc: DHT
      - 0xffdd: DRI
      - 0xffde: DNL
      - 0xffdf: DRI
      - 0xffe0: APP0
      - 0xffe1: APP1
      - 0xffe2: APP2
      - 0xffe3: APP3
      - 0xffe4: APP4
      - 0xffe5: APP5
      - 0xffe6: APP6
      - 0xffe7: APP7
      - 0xffe8: APP8
      - 0xffe9: APP9
      - 0xffea: APP10
      - 0xffeb: APP11
      - 0xffec: APP12
      - 0xffed: APP13
      - 0xffee: APP14
      - 0xffef: APP15
      - 0xfff0: JPG0
      - 0xfff1: JPG1
      - 0xfff2: JPG2
      - 0xfff3: JPG3
      - 0xfff4: JPG4
      - 0xfff5: JPG5
      - 0xfff6: JPG6
      - 0xfff7: JPG7
      - 0xfff8: JPG8
      - 0xfff9: JPG9
      - 0xfffa: JPG10
      - 0xfffb: JPG11
      - 0xfffc: JPG12
      - 0xfffd: JPG13
      - 0xfffe: JPG14
      - 0xffff: JPG15


seq:
  - id: soi
    type: marker
    enum: SOI
  - id: segments
    type: seq
    repeat: eos
    seq:
      - id: marker_byte
        type: u1
      - id: marker
        type: marker
      - id: length
        type: u2be
      - id: data
        type: bytes
        size: length - 2
  - id: eoi
    type: marker
    enum: EOI

