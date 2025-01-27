meta:
  id: jpeg
  file-extension: jpg
  endian: be
seq:
  - id: soi
    type: u2
    valid: 0xFFD8
  - id: segments
    type: segment
    repeat: eos
types:
  segment:
    seq:
      - id: marker
        type: u2
      - id: length
        type: u2
        if: marker != 0xFFDA and marker != 0xFFD9
      - id: data
        size: length - 2
        if: marker != 0xFFDA and marker != 0xFFD9
      - id: scan_data
        size-eos: true
        if: marker == 0xFFDA
    instances:
      marker_name:
        value: >
          marker == 0xFFC0 ? "SOF0" :
          marker == 0xFFC1 ? "SOF1" :
          marker == 0xFFC2 ? "SOF2" :
          marker == 0xFFC3 ? "SOF3" :
          marker == 0xFFC4 ? "DHT" :
          marker == 0xFFC5 ? "SOF5" :
          marker == 0xFFC6 ? "SOF6" :
          marker == 0xFFC7 ? "SOF7" :
          marker == 0xFFC8 ? "JPG" :
          marker == 0xFFC9 ? "SOF9" :
          marker == 0xFFCA ? "SOF10" :
          marker == 0xFFCB ? "SOF11" :
          marker == 0xFFCC ? "DAC" :
          marker == 0xFFCD ? "SOF13" :
          marker == 0xFFCE ? "SOF14" :
          marker == 0xFFCF ? "SOF15" :
          marker == 0xFFD0 ? "RST0" :
          marker == 0xFFD1 ? "RST1" :
          marker == 0xFFD2 ? "RST2" :
          marker == 0xFFD3 ? "RST3" :
          marker == 0xFFD4 ? "RST4" :
          marker == 0xFFD5 ? "RST5" :
          marker == 0xFFD6 ? "RST6" :
          marker == 0xFFD7 ? "RST7" :
          marker == 0xFFD8 ? "SOI" :
          marker == 0xFFD9 ? "EOI" :
          marker == 0xFFDA ? "SOS" :
          marker == 0xFFDB ? "DQT" :
          marker == 0xFFDC ? "DNL" :
          marker == 0xFFDD ? "DRI" :
          marker == 0xFFDE ? "DHP" :
          marker == 0xFFDF ? "EXP" :
          marker == 0xFFE0 ? "APP0" :
          marker == 0xFFE1 ? "APP1" :
          marker == 0xFFE2 ? "APP2" :
          marker == 0xFFE3 ? "APP3" :
          marker == 0xFFE4 ? "APP4" :
          marker == 0xFFE5 ? "APP5" :
          marker == 0xFFE6 ? "APP6" :
          marker == 0xFFE7 ? "APP7" :
          marker == 0xFFE8 ? "APP8" :
          marker == 0xFFE9 ? "APP9" :
          marker == 0xFFEA ? "APP10" :
          marker == 0xFFEB ? "APP11" :
          marker == 0xFFEC ? "APP12" :
          marker == 0xFFED ? "APP13" :
          marker == 0xFFEE ? "APP14" :
          marker == 0xFFEF ? "APP15" :
          marker == 0xFFF0 ? "JPG0" :
          marker == 0xFFF1 ? "JPG1" :
          marker == 0xFFF2 ? "JPG2" :
          marker == 0xFFF3 ? "JPG3" :
          marker == 0xFFF4 ? "JPG4" :
          marker == 0xFFF5 ? "JPG5" :
          marker == 0xFFF6 ? "JPG6" :
          marker == 0xFFF7 ? "JPG7" :
          marker == 0xFFF8 ? "JPG8" :
          marker == 0xFFF9 ? "JPG9" :
          marker == 0xFFFA ? "JPG10" :
          marker == 0xFFFB ? "JPG11" :
          marker == 0xFFFC ? "JPG12" :
          marker == 0xFFFD ? "JPG13" :
          marker == 0xFFFE ? "COM" :
          "UNKNOWN"