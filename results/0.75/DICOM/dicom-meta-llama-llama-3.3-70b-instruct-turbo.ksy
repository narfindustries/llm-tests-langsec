meta:
  id: dicom
  title: Digital Imaging and Communications in Medicine (DICOM)
  file-extension: dcm
  endian: be

seq:
  - id: preamble
    size: 128

  - id: prefix
    size: 4
    type: str
    enum: [ 'DICM' ]

  - id: meta-information
    type: meta-information-group

types:
  meta-information-group:
    seq:
      - id: group-0002
        type: data-element

      - id: transfer-syntax-uid
        type: data-element

      - id: implementation-class-uid
        type: data-element

      - id: implementation-version-name
        type: data-element

      - id: source-application-entity-title
        type: data-element

      - id: private-implementation
        type: data-element

  data-element:
    seq:
      - id: tag
        size: 4

      - id: vr
        size: 2

      - id: length
        type: uint16

      - id: value
        size: lambda: length
        type: switch-on: vr
        cases:
          AE: str
          AS: str
          AT: uint16
          CS: str
          DA: str
          DS: str
          DT: str
          FL: float
          FD: float
          IS: str
          LO: str
          LT: str
          OB: seq:
            - id: item
              type: byte
          OD: seq:
            - id: item
              type: byte
          OF: seq:
            - id: item
              type: byte
          OL: seq:
            - id: item
              type: byte
          OV: seq:
            - id: item
              type: byte
          PN: str
          SH: str
          SL: int32
          SS: int16
          ST: str
          TM: str
          UI: str
          UL: uint32
          UN: seq:
            - id: item
              type: byte
          UR: str
          US: uint16
          UV: seq:
            - id: item
              type: byte