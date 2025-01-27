meta:
  id: dicom
  file-extension: dcm
  endian: le
  title: "Digital Imaging and Communications in Medicine (DICOM)"
  license: CC0-1.0
  ks-version: 0.9

doc: |
  DICOM is the standard for the communication and management of medical imaging information and related data. DICOM is most commonly used for storing and transmitting medical images enabling the integration of medical imaging devices such as scanners, servers, workstations, printers, network hardware, and PACS (Picture Archiving and Communication Systems) from multiple manufacturers. It has been widely adopted by hospitals and is making inroads into smaller applications like dentists' and doctors' offices.

seq:
  - id: preamble
    size: 0x80
  - id: magic
    contents: [0x44, 0x49, 0x43, 0x4d] # "DICM"
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
      - id: length
        type: u4
      - id: value
        size: length
    instances:
      has_reserved_field:
        value: vr in ['OB', 'OW', 'OF', 'SQ', 'UT', 'UN']