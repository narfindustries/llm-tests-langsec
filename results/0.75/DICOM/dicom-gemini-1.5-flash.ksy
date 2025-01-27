meta:
  id: dicom-gemini-1
  title: DICOM GEMINI 1.5 Flash
  homepage: ""
  authors: []
  license: ""
  compiler_options:
    python:
      module_name: dicom_gemini_1

types:
  - id: dicom_header
    seq:
      - id: magic
        type: u4
        doc: "Magic number"
      - id: version
        type: u2
        doc: "Version number"
      - id: flags
        type: u2
        doc: "Flags"
      - id: data_size
        type: u4
        doc: "Size of the data section"

  - id: dicom_data
    seq:
      - id: image_data
        type: u1
        repeat: expr(this.parent.header.data_size)
        doc: "Image data"


seq:
  - id: header
    type: dicom_header
  - id: data
    type: dicom_data

