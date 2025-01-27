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
    doc: The preamble is reserved for file meta information and should be set to all 0s.
  - id: magic
    contents: [0x44, 0x49, 0x43, 0x4D] # ASCII "DICM"
    doc: The DICOM prefix that identifies the file as a DICOM file.
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
        doc: Value Representation (VR) that defines the data type and format of the associated data element.
      - id: reserved
        type: u2
        if: has_reserved_field
      - id: value_length
        type: u4
      - id: value_field
        size: value_length
    instances:
      has_reserved_field:
        value: vr in ['OB', 'OW', 'OF', 'SQ', 'UT', 'UN']