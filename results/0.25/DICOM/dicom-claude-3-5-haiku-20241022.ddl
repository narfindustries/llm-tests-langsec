def dicom_file = struct {
    preamble: [u8; 128],
    prefix: [u8; 4],
    meta_header: dicom_meta_header,
    dataset: dicom_dataset
}

def dicom_meta_header = struct {
    group_length: u32,
    file_meta_info_version: [u8; 2],
    media_storage_sop_class_uid: string,
    media_storage_sop_instance_uid: string,
    transfer_syntax_uid: string,
    implementation_class_uid: string,
    implementation_version_name: string
}

def dicom_dataset = struct {
    patient_module: patient_module,
    study_module: study_module,
    series_module: series_module,
    image_module: image_module,
    optional_modules: optional_modules
}

def patient_module = struct {
    patient_name: string,
    patient_id: string,
    patient_birth_date: string,
    patient_sex: patient_sex_enum,
    patient_age: option<string>,
    patient_weight: option<f32>,
    ethnic_group: option<string>
}

enum patient_sex_enum {
    Male,
    Female,
    Other
}

def study_module = struct {
    study_instance_uid: string,
    study_date: string,
    study_time: string,
    referring_physician_name: option<string>,
    study_id: string,
    accession_number: option<string>,
    study_description: option<string>
}

def series_module = struct {
    modality: modality_enum,
    series_instance_uid: string,
    series_number: u16,
    series_description: option<string>,
    body_part_examined: option<string>,
    protocol_name: option<string>
}

enum modality_enum {
    CT,
    MR,
    US,
    XA,
    CR,
    DR,
    RF,
    NM,
    PET
}

def image_module = struct {
    rows: u16,
    columns: u16,
    pixel_spacing: pixel_spacing,
    bits_allocated: u8,
    bits_stored: u8,
    high_bit: u8,
    pixel_representation: pixel_representation_enum,
    window_center: option<f32>,
    window_width: option<f32>,
    pixel_data: bytes
}

def pixel_spacing = struct {
    row_spacing: f32,
    column_spacing: f32
}

enum pixel_representation_enum {
    Unsigned,
    Signed
}

def optional_modules = struct {
    equipment_module: option<equipment_module>,
    overlay_module: option<overlay_module>,
    waveform_module: option<waveform_module>
}

def equipment_module = struct {
    manufacturer: string,
    institution_name: option<string>,
    station_name: option<string>,
    software_versions: option<string>
}

def overlay_module = struct {
    overlay_rows: u16,
    overlay_columns: u16,
    overlay_type: overlay_type_enum,
    overlay_data: bytes
}

enum overlay_type_enum {
    Graphics,
    ROI
}

def waveform_module = struct {
    number_of_waveform_channels: u16,
    waveform_bits_allocated: u8,
    waveform_sample_interpretation: waveform_sample_type,
    waveform_data: bytes
}

enum waveform_sample_type {
    SignedShort,
    UnsignedShort,
    Float
}