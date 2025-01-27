meta:
  id: jpeg
  title: JPEG File Format
  file-extension: jpg
  xref:
    mime: [image/jpeg]
  endian: be

doc: |
  JPEG is a commonly used method of lossy compression for digital images,
  particularly for those images produced by digital photography.

seq:
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.marker == 0xffd9

types:
  segment:
    seq:
      - id: marker
        type: u2
      - id: length
        type: u2
        if: marker != 0xffd8 and marker != 0xffd9
        - id: data
        size: length - 2
        if: marker != 0xffd8 and marker != 0xffd9
    instances:
      soi:
        value: 0xffd8
      eoi:
        value: 0xffd9
      app0:
        value: 0xffe0
      dqt:
        value: 0xffdb
      sof0:
        value: 0xffc0
      dht:
        value: 0xffc4
      sos:
        value: 0xffda
      com:
        value: 0xfffe
    doc: |
      A segment in JPEG file, consisting of a marker and optional payload data.
      The Start Of Image (SOI) and End Of Image (EOI) markers do not contain data.