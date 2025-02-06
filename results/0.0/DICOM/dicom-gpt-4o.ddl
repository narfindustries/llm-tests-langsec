namespace DICOM;

// Define basic types
type UID = string;
type Date = string;
type Time = string;
type PersonName = string;
type String = string;
type IntegerString = string;
type DecimalString = string;

// DICOM Data Element
struct DataElement {
    tag: uint16[2];
    vr: string[2];
    length: uint32;
    value: bytes(length);
};

// DICOM File Meta Information
struct FileMetaInformation {
    groupLength: uint32;
    sopClassUID: UID;
    sopInstanceUID: UID;
    transferSyntaxUID: UID;
    implementationClassUID: UID;
    implementationVersionName: String;
};

// DICOM Patient Module
struct PatientModule {
    patientName: PersonName;
    patientID: String;
    patientBirthDate: Date;
    patientSex: String;
};

// DICOM Study Module
struct StudyModule {
    studyInstanceUID: UID;
    studyDate: Date;
    studyTime: Time;
    referringPhysicianName: PersonName;
    studyID: String;
    accessionNumber: String;
};

// DICOM Series Module
struct SeriesModule {
    seriesInstanceUID: UID;
    seriesNumber: IntegerString;
    modality: String;
    seriesDescription: String;
};

// DICOM Image Module
struct ImageModule {
    imageType: String;
    instanceNumber: IntegerString;
    patientOrientation: String;
    imagePositionPatient: DecimalString[3];
    imageOrientationPatient: DecimalString[6];
    pixelSpacing: DecimalString[2];
};

// DICOM Dataset
struct DICOMDataset {
    fileMetaInformation: FileMetaInformation;
    patientModule: PatientModule;
    studyModule: StudyModule;
    seriesModule: SeriesModule;
    imageModule: ImageModule;
    dataElements: DataElement[];
};