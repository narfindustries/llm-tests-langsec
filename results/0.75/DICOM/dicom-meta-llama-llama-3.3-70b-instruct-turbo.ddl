format dicom 3.0

type patient_name = string
type patient_id = string
type patient_birth_date = date
type patient_sex = enum { M, F, O }
type patient_age = string

type study_instance_uid = string
type study_date = date
type study_time = time
type study_description = string
type referring_physician_name = string

type series_instance_uid = string
type series_number = uint16
type series_description = string
type series_date = date
type series_time = time

type instance_number = uint16
type image_type = enum { ORIGINAL, DERIVED }
type image_orientation = enum { AXIAL, CORONAL, SAGITTAL }
type image_position = enum { FEET_FIRST, HEAD_FIRST }
type pixel_data = bytes

type modality = enum { CT, MR, US }
type modality_instance_uid = string

segment patient_information {
  patient_name: patient_name
  patient_id: patient_id
  patient_birth_date: patient_birth_date
  patient_sex: patient_sex
  patient_age: patient_age
}

segment study_information {
  study_instance_uid: study_instance_uid
  study_date: study_date
  study_time: study_time
  study_description: study_description
  referring_physician_name: referring_physician_name
}

segment series_information {
  series_instance_uid: series_instance_uid
  series_number: series_number
  series_description: series_description
  series_date: series_date
  series_time: series_time
}

segment image_information {
  instance_number: instance_number
  image_type: image_type
  image_orientation: image_orientation
  image_position: image_position
  pixel_data: pixel_data
}

segment modality_information {
  modality: modality
  modality_instance_uid: modality_instance_uid
}

segment dicom_file {
  transfer_syntax_uid: string
  media_storage_sop_instance_uid: string
  sop_instance_uid: string
  sop_class_uid: string
  patient_information: patient_information
  study_information: study_information
  series_information: series_information
  image_information: image_information
  modality_information: modality_information
}

segment dicom_file_header {
  file_size: uint32
  preamble: string
  dicom_file: dicom_file
}

segment dicom_directory_record {
  offset: uint32
  length_of_record: uint32
  record_type: string
}

segment dicom_directory {
  records: array[dicom_directory_record]
}

segment dicom_file_dataset {
  items: array[uint16]
  dicom_file: dicom_file
}

segment dicom_file_meta_information {
  transfer_syntax_uid: string
  media_storage_sop_instance_uid: string
  sop_instance_uid: string
  sop_class_uid: string
  patient_information: patient_information
  study_information: study_information
  series_information: series_information
  image_information: image_information
  modality_information: modality_information
}

segment dicom_file_pixel_data {
  pixel_data: bytes
}