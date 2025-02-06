def DicomFile {
    let FileHeader {
        preamble: u8[128]
        prefix: char[4] = "DICM"
    }

    let MetaInformation {
        group_length: u32
        file_meta_info_version: u8[2]
        media_storage_sop_class_uid: str
        media_storage_sop_instance_uid: str
        transfer_syntax_uid: str
        implementation_class_uid: str
        implementation_version_name: str [optional]
        source_application_entity_title: str [optional]
        private_information_creator_uid: str [optional]
        private_information: str [optional]
    }

    let PatientModule {
        patients_name: str @ 0x00100010
        patient_id: str @ 0x00100020
        patients_birth_date: str @ 0x00100030 [optional]
        patients_sex: enum u8 {
            M = 'M'
            F = 'F'
            O = 'O'
        } @ 0x00100040 [optional]
    }

    let StudyModule {
        study_instance_uid: str @ 0x0020000D
        study_date: str @ 0x00080020 [optional]
        study_time: str @ 0x00080030 [optional]
        referring_physicians_name: str @ 0x00080090 [optional]
        study_id: str @ 0x00200010 [optional]
        accession_number: str @ 0x00080050 [optional]
    }

    let SeriesModule {
        series_instance_uid: str @ 0x0020000E
        modality: enum str {
            CT = "CT"
            MR = "MR"
            US = "US"
            CR = "CR"
            DX = "DX"
            NM = "NM"
            PT = "PT"
            XA = "XA"
        } @ 0x00080060
        series_number: u16 @ 0x00200011 [optional]
        series_date: str @ 0x00080021 [optional]
        series_time: str @ 0x00080031 [optional]
    }

    let ImageModule {
        sop_instance_uid: str @ 0x00080018
        instance_number: u16 @ 0x00200013
        patient_orientation: str[2] @ 0x00200020 [optional]
        image_position_patient: f32[3] @ 0x00200032 [optional]
        image_orientation_patient: f32[6] @ 0x00200037 [optional]
    }

    let ImagePixelModule {
        samples_per_pixel: u16 @ 0x00280002
        photometric_interpretation: enum str {
            MONOCHROME1 = "MONOCHROME1"
            MONOCHROME2 = "MONOCHROME2"
            RGB = "RGB"
            YBR_FULL = "YBR_FULL"
        } @ 0x00280004
        rows: u16 @ 0x00280010
        columns: u16 @ 0x00280011
        bits_allocated: u16 @ 0x00280100
        bits_stored: u16 @ 0x00280101
        high_bit: u16 @ 0x00280102
        pixel_representation: u16 @ 0x00280103
        pixel_data: u8[] @ 0x7FE00010
    }

    let SOPCommonModule {
        sop_class_uid: str @ 0x00080016
        sop_instance_uid: str @ 0x00080018
        specific_character_set: str @ 0x00080005 [optional]
        instance_creation_date: str @ 0x00080012 [optional]
        instance_creation_time: str @ 0x00080013 [optional]
    }

    let EquipmentModule {
        manufacturer: str @ 0x00080070 [optional]
        institution_name: str @ 0x00080080 [optional]
        manufacturers_model_name: str @ 0x00081090 [optional]
        device_serial_number: str @ 0x00181000 [optional]
        software_versions: str[] @ 0x00181020 [optional]
    }
}