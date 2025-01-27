meta:
  id: jpeg
  file-extension: jpg
  endian: be

seq:
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.marker == 0xffd9

types:
  segment:
    seq:
      - id: marker_prefix
        contents: [0xff]
      - id: marker
        type: u1
      - id: length
        type: u2
        if: marker != 0xd9
      - id: data
        size: length - 2
        if: marker != 0xd9 and marker != 0xd8
    instances:
      marker_name:
        value: >-
          marker == 0xd8 ? "SOI" :
          marker == 0xd9 ? "EOI" :
          marker == 0xda ? "SOS" :
          marker == 0xe0 ? "APP0" :
          marker == 0xe1 ? "APP1" :
          marker == 0xe2 ? "APP2" :
          marker == 0xdb ? "DQT" :
          marker == 0xc0 ? "SOF0" :
          marker == 0xc2 ? "SOF2" :
          marker == 0xc4 ? "DHT" :
          marker == 0xfe ? "COM" :
          "Unknown"

    enums:
      markers:
        0xd8: soi
        0xd9: eoi  
        0xda: sos
        0xe0: app0
        0xe1: app1
        0xe2: app2
        0xdb: dqt
        0xc0: sof0
        0xc2: sof2
        0xc4: dht
        0xfe: com