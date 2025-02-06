meta:
  id: dicom
  file-extension: dcm
  endian: le
  title: Digital Imaging and Communications in Medicine (DICOM)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  DICOM is the standard for the communication and management of medical imaging information and related data. DICOM is most commonly used for storing and transmitting medical images enabling the integration of medical imaging devices such as scanners, servers, workstations, printers, network hardware, and PACS (Picture Archiving and Communication Systems) from multiple manufacturers. It has been widely adopted by hospitals and is making inroads into smaller applications like dentists' and doctors' offices.

seq:
  - id: preamble
    size: 128
    contents: [0x00]

  - id: prefix
    size: 4
    contents: "DICM"

  - id: elements
    type: data_element
    repeat: eos

types:
  data_element:
    seq:
      - id: tag_group
        type: u2
      - id: tag_element
        type: u2
      - id: vr
        type: str
        size: 2
        encoding: ASCII
      - id: reserved
        type: u2
        if: has_reserved_field
      - id: value_length
        type: u4
      - id: value_field
        size: value_length
        type:
          switch-on: vr
          cases:
            '"AE"': strz
            '"AS"': strz
            '"AT"': u4
            '"CS"': strz
            '"DA"': strz
            '"DS"': strz
            '"DT"': strz
            '"FL"': f4
            '"FD"': f8
            '"IS"': strz
            '"LO"': strz
            '"LT"': strz
            '"OB"': ob_value
            '"OD"': f8
            '"OF"': f4
            '"OW"': ow_value
            '"PN"': strz
            '"SH"': strz
            '"SL"': s4
            '"SQ"': sequence
            '"SS"': s2
            '"ST"': strz
            '"TM"': strz
            '"UC"': strz
            '"UI"': strz
            '"UL"': u4
            '"UN"': un_value
            '"UR"': strz
            '"US"': u2
            '"UT"': strz

    instances:
      has_reserved_field:
        value: vr in ["OB", "OW", "OF", "SQ", "UT", "UN"]

  ob_value:
    seq:
      - id: data
        size-eos: true

  ow_value:
    seq:
      - id: data
        size-eos: true
        type: u2

  un_value:
    seq:
      - id: data
        size-eos: true

  sequence:
    seq:
      - id: items
        type: data_element
        repeat: eos