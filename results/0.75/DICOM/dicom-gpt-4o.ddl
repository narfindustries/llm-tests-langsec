enum Sex : u8 {
    M = 0x4D, // 'M'
    F = 0x46, // 'F'
    O = 0x4F  // 'O'
}

struct DICOM {
    // File Meta Information Group
    struct FileMetaInformation {
        string file_meta_information_version; // (0002,0001)
        string media_storage_sop_class_uid;   // (0002,0002)
        string media_storage_sop_instance_uid; // (0002,0003)
        string transfer_syntax_uid;           // (0002,0010)
        string implementation_class_uid;      // (0002,0012)
        string implementation_version_name;   // (0002,0013)
        optional string source_application_entity_title; // (0002,0016)
    }
    
    // Patient Information
    struct PatientInformation {
        string patient_name;                  // (0010,0010)
        string patient_id;                    // (0010,0020)
        string patient_birth_date;            // (0010,0030)
        Sex patient_sex;                      // (0010,0040)
    }

    // Study Information
    struct StudyInformation {
        string study_instance_uid;            // (0020,000D)
        string study_date;                    // (0008,0020)
        optional string study_time;           // (0008,0030)
        optional string referring_physician_name; // (0008,0090)
    }

    // Series Information
    struct SeriesInformation {
        string series_instance_uid;           // (0020,000E)
        uint16 series_number;                 // (0020,0011)
        string modality;                      // (0008,0060)
    }

    // Image Information
    struct ImageInformation {
        string sop_instance_uid;              // (0008,0018)
        float64[3] image_position_patient;    // (0020,0032)
        float64[6] image_orientation_patient; // (0020,0037)
        float64[2] pixel_spacing;             // (0028,0030)
    }

    // Equipment Information
    struct EquipmentInformation {
        string manufacturer;                  // (0008,0070)
        optional string manufacturer_model_name; // (0008,1090)
    }

    // Acquisition Parameters
    struct AcquisitionParameters {
        optional float64 slice_thickness;     // (0018,0050)
        optional float64 kvp;                 // (0018,0060)
    }

    // Image Presentation
    struct ImagePresentation {
        optional float64 window_center;       // (0028,1050)
        optional float64 window_width;        // (0028,1051)
    }
}