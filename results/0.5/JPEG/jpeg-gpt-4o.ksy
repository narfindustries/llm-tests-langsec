meta:
  id: jpeg
  title: JPEG
  file-extension: jpg
  endian: be

seq:
  - id: segments
    type: segment
    repeat: until
    repeat-until: _.marker == 'ffd9'

types:
  segment:
    seq:
      - id: marker
        type: u2
      - id: length
        type: u2
        if: marker != 'ffd8' and marker != 'ffd9'
      - id: data
        size: length - 2
        if: marker != 'ffd8' and marker != 'ffd9'

    instances:
      marker_name:
        value: |
          switch marker {
            0xffd8 => "soi",
            0xffe0 => "app0",
            0xffe1 => "app1",
            0xffe2 => "app2",
            0xffe3 => "app3",
            0xffe4 => "app4",
            0xffe5 => "app5",
            0xffe6 => "app6",
            0xffe7 => "app7",
            0xffe8 => "app8",
            0xffe9 => "app9",
            0xffea => "app10",
            0xffeb => "app11",
            0xffec => "app12",
            0xffed => "app13",
            0xffee => "app14",
            0xffef => "app15",
            0xfffe => "com",
            0xffdb => "dqt",
            0xffc0 => "sof0",
            0xffc1 => "sof1",
            0xffc2 => "sof2",
            0xffc3 => "sof3",
            0xffc4 => "dht",
            0xffc5 => "sof5",
            0xffc6 => "sof6",
            0xffc7 => "sof7",
            0xffc8 => "jpg",
            0xffc9 => "sof9",
            0xffca => "sof10",
            0xffcb => "sof11",
            0xffcc => "dac",
            0xffcd => "sof13",
            0xffce => "sof14",
            0xffcf => "sof15",
            0xffda => "sos",
            0xffdd => "dri",
            0xffdc => "dnl",
            0xffde => "dhp",
            0xffdf => "exp",
            0xffd0 => "rst0",
            0xffd1 => "rst1",
            0xffd2 => "rst2",
            0xffd3 => "rst3",
            0xffd4 => "rst4",
            0xffd5 => "rst5",
            0xffd6 => "rst6",
            0xffd7 => "rst7",
            0xffd9 => "eoi"
          }