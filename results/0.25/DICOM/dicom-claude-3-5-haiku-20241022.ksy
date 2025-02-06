meta:
  id: dicom
  file-extension: dcm
  endian: le
  encoding: UTF-8

seq:
  - id: preamble
    size: 128
    doc: "DICOM file preamble"

  - id: dicom_prefix
    type: str
    size: 4
    encoding: ASCII
    doc: "DICOM file prefix (DICM)"

  - id: meta_header
    type: meta_information
    doc: "DICOM meta information header"

  - id: dataset
    type: dicom_dataset
    doc: "DICOM dataset containing patient and image information"

types:
  meta_information:
    seq:
      - id: file_meta_information_group_length
        type: attribute
      - id: file_meta_information_version
        type: attribute
      - id: media_storage_sop_class_uid
        type: attribute
      - id: media_storage_sop_instance_uid
        type: attribute
      - id: transfer_syntax_uid
        type: attribute
      - id: implementation_class_uid
        type: attribute
      - id: implementation_version_name
        type: attribute

  dicom_dataset:
    seq:
      - id: patient_module
        type: patient_module
      - id: study_module
        type: study_module
      - id: series_module
        type: series_module
      - id: image_module
        type: image_module

  patient_module:
    seq:
      - id: patient_name
        type: attribute
        doc: "Patient's full name"
      - id: patient_id
        type: attribute
        doc: "Patient identifier"
      - id: patient_birth_date
        type: attribute
        doc: "Patient's date of birth"
      - id: patient_sex
        type: attribute
        doc: "Patient's sex (M/F/O)"
      - id: patient_age
        type: attribute
        doc: "Patient's age at time of study"

  study_module:
    seq:
      - id: study_instance_uid
        type: attribute
      - id: study_date
        type: attribute
      - id: study_time
        type: attribute
      - id: accession_number
        type: attribute
      - id: referring_physician_name
        type: attribute
      - id: study_description
        type: attribute

  series_module:
    seq:
      - id: modality
        type: attribute
      - id: series_instance_uid
        type: attribute
      - id: series_number
        type: attribute
      - id: series_description
        type: attribute
      - id: body_part_examined
        type: attribute

  image_module:
    seq:
      - id: sop_instance_uid
        type: attribute
      - id: image_number
        type: attribute
      - id: pixel_data
        type: pixel_data
      - id: rows
        type: attribute
      - id: columns
        type: attribute
      - id: bits_allocated
        type: attribute
      - id: photometric_interpretation
        type: attribute

  pixel_data:
    seq:
      - id: length
        type: u4
      - id: compressed_data
        size: length
        doc: "Compressed or uncompressed pixel data"

  attribute:
    seq:
      - id: tag
        type: u4
      - id: vr
        type: str
        size: 2
      - id: length
        type: u2
      - id: value
        size: length
        type: str
        encoding: UTF-8