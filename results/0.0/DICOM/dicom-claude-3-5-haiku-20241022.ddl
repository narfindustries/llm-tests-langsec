type DICOM_File = {
    header: DICOM_Header,
    pixel_data: Pixel_Data
}

type DICOM_Header = {
    transfer_syntax: Transfer_Syntax,
    patient_info: Patient_Info,
    study_info: Study_Info,
    series_info: Series_Info
}

type Transfer_Syntax = {
    uid: String,
    explicit_vr: Bool,
    little_endian: Bool
}

type Patient_Info = {
    patient_id: String,
    patient_name: String,
    patient_birthdate: String,
    patient_sex: String
}

type Study_Info = {
    study_instance_uid: String,
    study_date: String,
    study_description: String
}

type Series_Info = {
    series_instance_uid: String,
    series_number: Int,
    modality: String
}

type Pixel_Data = {
    rows: Int,
    columns: Int,
    bits_allocated: Int,
    bits_stored: Int,
    pixel_representation: Int,
    samples_per_pixel: Int,
    photometric_interpretation: String,
    pixel_data_bytes: Bytes
}

parser dicom_file = 
    header:dicom_header 
    pixel_data:pixel_data_section 
    => { header, pixel_data }

parser dicom_header = 
    transfer_syntax:transfer_syntax_section
    patient_info:patient_info_section
    study_info:study_info_section
    series_info:series_info_section
    => { 
        transfer_syntax, 
        patient_info, 
        study_info, 
        series_info 
    }

parser transfer_syntax_section = 
    uid:transfer_syntax_uid
    explicit_vr:is_explicit_vr
    little_endian:is_little_endian
    => { uid, explicit_vr, little_endian }

parser patient_info_section = 
    patient_id:patient_id_tag
    patient_name:patient_name_tag
    patient_birthdate:patient_birthdate_tag
    patient_sex:patient_sex_tag
    => { patient_id, patient_name, patient_birthdate, patient_sex }

parser study_info_section = 
    study_instance_uid:study_instance_uid_tag
    study_date:study_date_tag
    study_description:study_description_tag
    => { study_instance_uid, study_date, study_description }

parser series_info_section = 
    series_instance_uid:series_instance_uid_tag
    series_number:series_number_tag
    modality:modality_tag
    => { series_instance_uid, series_number, modality }

parser pixel_data_section = 
    rows:rows_tag
    columns:columns_tag
    bits_allocated:bits_allocated_tag
    bits_stored:bits_stored_tag
    pixel_representation:pixel_representation_tag
    samples_per_pixel:samples_per_pixel_tag
    photometric_interpretation:photometric_interpretation_tag
    pixel_data_bytes:pixel_data_bytes_tag
    => { 
        rows, 
        columns, 
        bits_allocated, 
        bits_stored, 
        pixel_representation, 
        samples_per_pixel, 
        photometric_interpretation, 
        pixel_data_bytes 
    }