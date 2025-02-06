-- This is NOT a complete DICOM Daedalus specification.  It is impossible to create one of that scale here.
-- This is a placeholder illustrating a VERY small portion of DICOM's structure.

module DICOM_Partial

  record PatientName {
    family_name : string;
    given_name : string;
    middle_name : string?; -- Optional
    prefix : string?;     -- Optional
    suffix : string?;      -- Optional
  }

  record PatientRecord {
    patient_name : PatientName;
    patient_id : string;
    patient_birthdate : date;
  }

  top : PatientRecord;

