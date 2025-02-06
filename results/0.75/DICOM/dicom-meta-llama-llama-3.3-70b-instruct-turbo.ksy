meta:
  id: dicom
  title: Digital Imaging and Communications in Medicine
seq:
  - id: file_meta_information_group
    type: file_meta_information
  - id: transfer_syntax_uid
    size: 64
  - id: implementation_class_uid
    size: 64
  - id: content
    type: dicom_data_set

types:
  file_meta_information:
    seq:
      - id: file_meta_information_version
        size: 2
      - id: media_storage_sop_instance_uid
        size: 64
      - id: media_storage_sop_class_uid
        size: 64
      - id: transfer_syntax_uid
        size: 64
      - id: implementation_class_uid
        size: 64
      - id: source_application_entity_title
        size: 64
      - id: sending_application_entity_title
        size: 64
      - id: receiving_application_entity_title
        size: 64
      - id: private_implementation_data
        size: 64

  dicom_data_set:
    seq:
      - id: patient_information
        type: patient_information
      - id: study_information
        type: study_information
      - id: series_information
        type: series_information
      - id: image_information
        type: image_information
      - id: pixel_data
        type: pixel_data
      - id: overlay_data
        type: overlay_data
      - id: curve_data
        type: curve_data
      - id: wave_data
        type: wave_data
      - id: private_data
        type: private_data

  patient_information:
    seq:
      - id: patient_name
        size: 64
      - id: patient_id
        size: 64
      - id: patient_birth_date
        size: 8
      - id: patient_sex
        size: 1
      - id: patient_age
        size: 2
      - id: patient_weight
        size: 4
      - id: patient_address
        size: 64
      - id: patient_phone
        size: 64

  study_information:
    seq:
      - id: study_instance_uid
        size: 64
      - id: study_date
        size: 8
      - id: study_time
        size: 6
      - id: study_id
        size: 64
      - id: study_description
        size: 64
      - id: study_status
        size: 1

  series_information:
    seq:
      - id: series_instance_uid
        size: 64
      - id: series_date
        size: 8
      - id: series_time
        size: 6
      - id: series_number
        size: 2
      - id: series_description
        size: 64
      - id: series_status
        size: 1

  image_information:
    seq:
      - id: sop_instance_uid
        size: 64
      - id: image_type
        size: 64
      - id: image_date
        size: 8
      - id: image_time
        size: 6
      - id: image_number
        size: 2
      - id: image_description
        size: 64
      - id: patient_position
        size: 1
      - id: image_orientation
        size: 6

  pixel_data:
    seq:
      - id: pixel_data
        size: _parent._io.size - _parent._io.pos

  overlay_data:
    seq:
      - id: overlay_data
        size: _parent._io.size - _parent._io.pos

  curve_data:
    seq:
      - id: curve_data
        size: _parent._io.size - _parent._io.pos

  wave_data:
    seq:
      - id: wave_data
        size: _parent._io.size - _parent._io.pos

  private_data:
    seq:
      - id: private_data
        size: _parent._io.size - _parent._io.pos