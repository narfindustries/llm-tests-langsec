struct NITF {
    magic: Magic;
    header: Header;
    imageSegments: ImageSegment[];
    textSegments: TextSegment[];
    dataExtensions: DataExtension[];
}

struct Magic {
    value: uint8[4] = [0x4E, 0x49, 0x54, 0x46]; // "NITF"
}

struct Header {
    fileVersion: uint8[2];
    complexLevel: uint8;
    standardType: uint8;
    originStationID: uint8[10];
    fileDateTime: uint8[14];
    fileTitle: uint8[80];
    securityClassification: uint8[1];
    classificationSystem: uint8[2];
    codewords: uint8[11];
    controlAndHandling: uint8[2];
    releaseInstructions: uint8[20];
    declassificationType: uint8[2];
    declassificationDate: uint8[8];
    declassificationExemption: uint8[4];
    downgrade: uint8[1];
    downgradeDate: uint8[8];
    classificationText: uint8[43];
    classificationAuthorityType: uint8[1];
    classificationAuthority: uint8[40];
    classificationReason: uint8[1];
    securitySourceDate: uint8[8];
    securityControlNumber: uint8[15];
    fileCopyNumber: uint8[5];
    fileNumberOfCopies: uint8[5];
    encryption: uint8[1];
    fileBackgroundColor: uint8[3];
    originatorName: uint8[24];
    originatorPhone: uint8[18];
    fileLength: uint32;
    headerLength: uint16;
    numberOfImageSegments: uint16;
    imageSegmentLengths: uint32[numberOfImageSegments];
    numberOfTextSegments: uint16;
    textSegmentLengths: uint32[numberOfTextSegments];
    numberOfDataExtensions: uint16;
    dataExtensionLengths: uint32[numberOfDataExtensions];
    reserved: uint8[6];
}

struct ImageSegment {
    subheader: ImageSubheader;
    data: uint8[imageSegmentLengths[index]];
}

struct ImageSubheader {
    imageID: uint8[10];
    imageDateTime: uint8[14];
    targetID: uint8[17];
    imageTitle: uint8[80];
    securityClassification: uint8[1];
    classificationSystem: uint8[2];
    codewords: uint8[11];
    controlAndHandling: uint8[2];
    releaseInstructions: uint8[20];
    declassificationType: uint8[2];
    declassificationDate: uint8[8];
    declassificationExemption: uint8[4];
    downgrade: uint8[1];
    downgradeDate: uint8[8];
    classificationText: uint8[43];
    classificationAuthorityType: uint8[1];
    classificationAuthority: uint8[40];
    classificationReason: uint8[1];
    securitySourceDate: uint8[8];
    securityControlNumber: uint8[15];
    imageSource: uint8[42];
    imageCompression: uint8[2];
    imageBandCount: uint8[1];
    imageBandInfo: uint8[imageBandCount * 6];
    imageColor: uint8[1];
    imageDataFormat: uint8[3];
    imageMagnification: uint8[4];
    imageComments: uint8[80];
    imageDataLength: uint32;
}

struct TextSegment {
    subheader: TextSubheader;
    data: uint8[textSegmentLengths[index]];
}

struct TextSubheader {
    textID: uint8[10];
    textDateTime: uint8[14];
    textTitle: uint8[80];
    securityClassification: uint8[1];
    classificationSystem: uint8[2];
    codewords: uint8[11];
    controlAndHandling: uint8[2];
    releaseInstructions: uint8[20];
    declassificationType: uint8[2];
    declassificationDate: uint8[8];
    declassificationExemption: uint8[4];
    downgrade: uint8[1];
    downgradeDate: uint8[8];
    classificationText: uint8[43];
    classificationAuthorityType: uint8[1];
    classificationAuthority: uint8[40];
    classificationReason: uint8[1];
    securitySourceDate: uint8[8];
    securityControlNumber: uint8[15];
    textFormat: uint8[3];
    textExtendedSubheader: uint8[1];
    textDataLength: uint32;
}

struct DataExtension {
    subheader: DataExtensionSubheader;
    data: uint8[dataExtensionLengths[index]];
}

struct DataExtensionSubheader {
    dataID: uint8[10];
    dataDateTime: uint8[14];
    dataTitle: uint8[80];
    securityClassification: uint8[1];
    classificationSystem: uint8[2];
    codewords: uint8[11];
    controlAndHandling: uint8[2];
    releaseInstructions: uint8[20];
    declassificationType: uint8[2];
    declassificationDate: uint8[8];
    declassificationExemption: uint8[4];
    downgrade: uint8[1];
    downgradeDate: uint8[8];
    classificationText: uint8[43];
    classificationAuthorityType: uint8[1];
    classificationAuthority: uint8[40];
    classificationReason: uint8[1];
    securitySourceDate: uint8[8];
    securityControlNumber: uint8[15];
    dataFormat: uint8[3];
    dataExtendedSubheader: uint8[1];
    dataDataLength: uint32;
}