struct NITFHeader {
    magic: uint8[4] = [0x4E, 0x49, 0x54, 0x46];
    version: uint8[2];
    complexity: uint8;
    standardType: uint8;
    originatingStationID: uint8[10];
    dateTime: uint8[14];
    fileTitle: uint8[80];
    securityClassification: uint8[1];
    securityClassificationSystem: uint8[2];
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
}

struct ImageSegment {
    subheader: ImageSubheader;
    data: uint8[..];
}

struct ImageSubheader {
    imageID: uint8[10];
    dateTime: uint8[14];
    targetID: uint8[17];
    imageTitle: uint8[80];
    securityClassification: uint8[1];
    securityClassificationSystem: uint8[2];
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
    imageDisplayLevel: uint8[3];
    attachmentLevel: uint8[3];
    imageLocation: uint8[10];
    imageMagnification: uint8[4];
    imageCompression: uint8[2];
    imageColor: uint8[1];
    imageFormat: uint8[1];
    numberOfRows: uint32;
    numberOfColumns: uint32;
    pixelValueType: uint8[3];
    imageRepresentation: uint8[8];
    imageCategory: uint8[8];
    actualBitsPerPixel: uint8[2];
    pixelJustification: uint8[1];
    imageCoordinateSystem: uint8[1];
    numberOfImageComments: uint8[1];
    imageComments: uint8[80 * numberOfImageComments];
}

struct NITF {
    header: NITFHeader;
    imageSegments: ImageSegment[..];
}