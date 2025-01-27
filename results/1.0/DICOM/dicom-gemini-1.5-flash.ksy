meta:
  id: dicom-gemini-1
  title: DICOM GEMINI 1.5 Flash
  homepage: https://www.dicomstandard.org/
  file_extension: .dcm
  experimental: true

types:
  DICOMHeader:
    seq:
      - id: preamble
        type: u1
        size: 128
      - id: magic_number
        type: str
        size: 4
      - id: affected_files
        type: DICOMAffectedFiles
      - id: group_length
        type: u4be
        size: 4
      - id: dataset
        type: DICOMDataset

  DICOMAffectedFiles:
    seq:
      - id: file_count
        type: u2be
      - id: files
        type: DICOMFile
        repeat: expr1
      
  DICOMFile:
    seq:
      - id: file_id
        type: u4be
      - id: file_name
        type: strz
      - id: file_path
        type: strz

  DICOMDataset:
    seq:
      - id: elements
        type: DICOMElement
        repeat: eos

  DICOMElement:
    seq:
      - id: group
        type: u2be
      - id: element
        type: u2be
      - id: vr
        type: str
        size: 2
      - id: value_length
        type: u4be
      - id: value
        type: bytes
        size: expr1

expr1:
  - value: this.file_count if isinstance(self, DICOMAffectedFiles) else this.value_length
