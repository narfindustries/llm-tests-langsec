DICOMFile {
    Preamble: bytes[128] = [0x00] * 128;
    Prefix: "DICM";
    Elements: Element[];
}

Element {
    Tag: uint16;
    VR: string[2];
    Length: uint32;
    Value: bytes[Length];
}

PatientModule {
    PatientName: Element (Tag == 0x00100010);
    PatientID: Element (Tag == 0x00100020);
    PatientBirthDate: Element (Tag == 0x00100030);
    PatientSex: Element (Tag == 0x00100040);
}

StudyModule {
    StudyInstanceUID: Element (Tag == 0x0020000D);
    StudyDate: Element (Tag == 0x00080020);
    StudyTime: Element (Tag == 0x00080030);
    ReferringPhysicianName: Element (Tag == 0x00080090);
}

SeriesModule {
    SeriesInstanceUID: Element (Tag == 0x0020000E);
    Modality: Element (Tag == 0x00080060);
    SeriesNumber: Element (Tag == 0x00200011);
}

ImageModule {
    SOPInstanceUID: Element (Tag == 0x00080018);
    ImagePositionPatient: Element (Tag == 0x00200032);
    ImageOrientationPatient: Element (Tag == 0x00200037);
    PixelSpacing: Element (Tag == 0x00280030);
}

FileMetaInformation {
    FileMetaInformationGroupLength: Element (Tag == 0x00020000);
    FileMetaInformationVersion: Element (Tag == 0x00020001);
    MediaStorageSOPClassUID: Element (Tag == 0x00020002);
    MediaStorageSOPInstanceUID: Element (Tag == 0x00020003);
    TransferSyntaxUID: Element (Tag == 0x00020010);
    ImplementationClassUID: Element (Tag == 0x00020012);
    ImplementationVersionName: Element (Tag == 0x00020013);
}

DICOMDataSet {
    FileMetaInformation: FileMetaInformation;
    PatientModule: PatientModule;
    StudyModule: StudyModule;
    SeriesModule: SeriesModule;
    ImageModule: ImageModule;
    OtherElements: Element[];
}