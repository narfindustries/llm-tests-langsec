meta:
  id: jpeg
  file-extension: jpg
  endian: be
  title: JPEG (Joint Photographic Experts Group) image
  license: CC0-1.0
  ks-version: 0.9

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
        type: u1
        enum: marker_enum
      - id: length
        type: u2
        if: marker != marker_enum::soi and marker != marker_enum::eoi
      - id: data
        size: length - 2
        if: length > 2

enums:
  marker_enum:
    0xd8: soi  # Start Of Image
    0xc0: sof0 # Start Of Frame (Baseline DCT)
    0xc2: sof2 # Start Of Frame (Progressive DCT)
    0xc4: dht  # Define Huffman Table
    0xdb: dqt  # Define Quantization Table
    0xdd: dri  # Define Restart Interval
    0xda: sos  # Start Of Scan
    0xfe: com  # Comment
    0xd9: eoi  # End Of Image
    0xe0: app0 # Application-specific (JFIF)
    0xe1: app1 # Application-specific (Exif)
    0xe2: app2 # Application-specific
    0xe3: app3 # Application-specific
    0xe4: app4 # Application-specific
    0xe5: app5 # Application-specific
    0xe6: app6 # Application-specific
    0xe7: app7 # Application-specific
    0xe8: app8 # Application-specific
    0xe9: app9 # Application-specific
    0xea: appa # Application-specific
    0xeb: appb # Application-specific
    0xec: appc # Application-specific
    0xed: appd # Application-specific
    0xee: appe # Application-specific
    0xef: appf # Application-specific
    0xf0: jpg0 # Reserved for JPEG extensions
    0xf1: jpg1 # Reserved for JPEG extensions
    0xf2: jpg2 # Reserved for JPEG extensions
    0xf3: jpg3 # Reserved for JPEG extensions
    0xf4: jpg4 # Reserved for JPEG extensions
    0xf5: jpg5 # Reserved for JPEG extensions
    0xf6: jpg6 # Reserved for JPEG extensions
    0xf7: jpg7 # Reserved for JPEG extensions
    0xf8: jpg8 # Reserved for JPEG extensions
    0xf9: jpg9 # Reserved for JPEG extensions
    0xfa: jpga # Reserved for JPEG extensions
    0xfb: jpgb # Reserved for JPEG extensions
    0xfc: jpgc # Reserved for JPEG extensions
    0xfd: jpgd # Reserved for JPEG extensions
    0x01: tem  # Temporary private use in arithmetic coding
    0xc3: sof3 # Start Of Frame (Lossless, sequential)
    0xc5: sof5 # Differential sequential DCT
    0xc6: sof6 # Differential progressive DCT
    0xc7: sof7 # Differential lossless (sequential)
    0xc8: jpg  # Reserved for JPEG extensions
    0xc9: sof9 # Extended sequential DCT
    0xca: sof10 # Progressive DCT
    0xcb: sof11 # Lossless (sequential)
    0xcd: sof13 # Differential sequential DCT
    0xce: sof14 # Differential progressive DCT
    0xcf: sof15 # Differential lossless (sequential)