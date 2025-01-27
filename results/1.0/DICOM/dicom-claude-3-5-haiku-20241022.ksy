meta:
  id: dicom
  file-extension: dcm
  title: Digital Imaging and Communications in Medicine (DICOM) File Format
  endian: implicit_little
  imports:
    - common_types

seq:
  - id: preamble
    size: 128
    doc: 128-byte preamble

  - id: magic_word
    type: str
    size: 4
    encoding: ASCII
    doc: DICOM magic word "DICM"

  - id: metadata_elements
    type: metadata_element
    repeat: eos
    doc: DICOM metadata elements

types:
  metadata_element:
    seq:
      - id: group
        type: u2
        doc: Group number of the element

      - id: element
        type: u2
        doc: Element number within the group

      - id: vr
        type: str
        size: 2
        encoding: ASCII
        doc: Value Representation (VR) of the element

      - id: length
        type:
          switch-on: vr
          cases:
            '"OB"': u4
            '"OW"': u4
            '"SQ"': u4
            '"UN"': u4
            _: u2
        doc: Length of the element's value

      - id: value
        size: length
        type:
          switch-on: vr
          cases:
            '"PN"': person_name
            '"UI"': unique_identifier
            '"DA"': date
            '"TM"': time
            '"CS"': code_string
            '"IS"': integer_string
            '"DS"': decimal_string
            _: raw_value
        doc: Actual value of the element

  person_name:
    seq:
      - id: components
        type: str
        terminator: 0x5E
        repeat: until
        repeat-until: _io.is_eof
        doc: Person name components (Last^First^Middle)

  unique_identifier:
    seq:
      - id: uid
        type: strz
        encoding: ASCII
        doc: Unique identifier string

  date:
    seq:
      - id: date_string
        type: strz
        encoding: ASCII
        doc: Date in YYYYMMDD format

  time:
    seq:
      - id: time_string
        type: strz
        encoding: ASCII
        doc: Time in HHMMSS.FFFFFF format

  code_string:
    seq:
      - id: value
        type: strz
        encoding: ASCII
        doc: Code string value

  integer_string:
    seq:
      - id: value
        type: strz
        encoding: ASCII
        doc: Integer string value

  decimal_string:
    seq:
      - id: value
        type: strz
        encoding: ASCII
        doc: Decimal string value

  raw_value:
    seq:
      - id: data
        size-eos: true
        doc: Raw byte data for unhandled value representations