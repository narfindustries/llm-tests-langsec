meta:
  id: jpeg
  title: JPEG File Format
  file-extension: jpg
  xref:
    mime: image/jpeg
  endian: be

doc: |
  JPEG is a commonly used method of lossy compression for digital images.

seq:
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.marker == marker_enum::eoi

types:
  segment:
    seq:
      - id: marker
        type: u2
        enum: marker_enum

      - id: length
        type: u2
        if: marker != marker_enum::sos && marker != marker_enum::eoi && !((marker >= 0xffe0) && (marker <= 0xffef))
        doc: |
          The length field specifies the length of the marker segment
          excluding the marker itself, but including the length field.

      # JPEG provides various types of segments, depending on marker
      - id: data
        size: length - 2
        if: marker != marker_enum::sos && marker != marker_enum::eoi && !((marker >= 0xffe0) && (marker <= 0xffef))

      - id: scan_data
        size-eos: true
        if: marker == marker_enum::sos

enums:
  marker_enum:
    0xffd8: soi
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
    0xffdb: dqt
    0xffc0: sof0
    0xffc2: sof2
    0xffc4: dht
    0xffda: sos
    0xfffe: com
    0xffd9: eoi