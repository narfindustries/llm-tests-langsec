meta:
  id: nitf
  endian: be

types:
  file_header:
    seq:
      - id: fhdrsz
        type: u1
      - id: fhdrtyp
        type: str
        size: 2
      - id: fhdrtitl
        type: str
        size: 25
      - id: fhdtim
        type: str
        size: 6
      - id: fhdseccl
        type: str
        size: 1
    instances:
      fhdrsz:
        value: 25
      fhdrtyp:
        value: "BF01"

  file_security_metadata:
    seq:
      - id: fsmver
        type: u1
      - id: fsmcls
        type: str
        size: 1
      - id: fsmctlh
        type: str
        size: 1
    instances:
      fsmver:
        value: 1

  image_header:
    seq:
      - id: iid1
        type: str
        size: 25
      - id: iid2
        type: str
        size: 17
      - id: isclas
        type: str
        size: 1
      - id: isctlh
        type: str
        size: 1
      - id: iic
        type: str
        size: 2

  image_compression:
    seq:
      - id: icver
        type: u1
      - id: ictype
        type: str
        size: 1

  image_data:
    type: u1
    repeat: eos

  graphics_header:
    seq:
      - id: gid
        type: str
        size: 25
      - id: gtype
        type: str
        size: 1

  graphics_data:
    type: u1
    repeat: eos

  text_header:
    seq:
      - id: tid
        type: str
        size: 25
      - id: ttype
        type: str
        size: 1

  text_data:
    type: u1
    repeat: eos

  file_content:
    seq:
      - id: file_security_metadata
        type: file_security_metadata
        if: file_header.fhdrsz == 25
      - id: image_header
        type: image_header
      - id: image_compression
        type: image_compression
        if: image_compression.icver == 1
      - id: image_data
        type: image_data
      - id: graphics_header
        type: graphics_header
        if: graphics_header.gtype in ["P", "L", "C", "R"]
      - id: graphics_data
        type: graphics_data
      - id: text_header
        type: text_header
        if: text_header.ttype in ["P", "F"]
      - id: text_data
        type: text_data

seq:
  - id: file_header
    type: file_header
  - id: file_content
    type: file_content