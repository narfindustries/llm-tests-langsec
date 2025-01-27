meta:
  id: dicom_deepseek_chat
  title: DICOM DeepSeek Chat Format
  file-extension: dicom
  endian: le
  license: MIT
  encoding: UTF-8

seq:
  - id: preamble
    size: 128
    doc: Preamble (128 bytes, usually zeros)
  - id: prefix
    contents: "DICM"
    doc: Prefix indicating DICOM format
  - id: data_elements
    type: data_element
    repeat: eos
    doc: Sequence of DICOM data elements

types:
  data_element:
    seq:
      - id: tag
        type: u2
        doc: Tag identifying the data element
      - id: vr
        type: str
        size: 2
        doc: Value Representation (VR) of the data element
      - id: length
        type: u2
        doc: Length of the value field
      - id: value
        size: length
        doc: Value of the data element