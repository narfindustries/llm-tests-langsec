meta:
  id: dicom
  file-extension: dcm
  endian: le

seq:
  - id: file_preamble
    size: 128
    
  - id: dicom_prefix
    contents: [0x44, 0x49, 0x43, 0x4D]

  - id: file_meta_information_group
    type: file_meta_info

  - id: dataset
    type: dataset_sequence

types:
  file_meta_info:
    seq:
      - id: group_length
        type: tag_element
      - id: transfer_syntax
        type: tag_element
      - id: sop_class_uid
        type: tag_element
      - id: sop_instance_uid
        type: tag_element
      - id: implementation_class_uid
        type: tag_element

  dataset_sequence:
    seq:
      - id: patient_tags
        type: patient_information
      - id: study_tags
        type: study_information
      - id: series_tags
        type: series_information
      - id: image_tags
        type: image_information

  patient_information:
    seq:
      - id: patient_name
        type: tag_element
      - id: patient_id
        type: tag_element
      - id: patient_birth_date
        type: tag_element
      - id: patient_sex
        type: tag_element
      - id: patient_age
        type: tag_element
        if: _parent._io.size > _io.pos

  study_information:
    seq:
      - id: study_instance_uid
        type: tag_element
      - id: study_date
        type: tag_element
      - id: study_time
        type: tag_element
      - id: accession_number
        type: tag_element
      - id: referring_physician
        type: tag_element
        if: _parent._io.size > _io.pos

  series_information:
    seq:
      - id: series_instance_uid
        type: tag_element
      - id: modality
        type: tag_element
      - id: series_number
        type: tag_element
      - id: series_description
        type: tag_element
        if: _parent._io.size > _io.pos

  image_information:
    seq:
      - id: sop_class_uid
        type: tag_element
      - id: sop_instance_uid
        type: tag_element
      - id: image_type
        type: tag_element
      - id: pixel_data
        type: pixel_sequence

  pixel_sequence:
    seq:
      - id: pixel_representation
        type: tag_element
      - id: rows
        type: tag_element
      - id: columns
        type: tag_element
      - id: bits_allocated
        type: tag_element
      - id: bits_stored
        type: tag_element
      - id: pixel_data_element
        type: tag_element

  tag_element:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2
      - id: len_value
        type: u4
      - id: value
        size: len_value