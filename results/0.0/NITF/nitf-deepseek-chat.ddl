struct NITF {
    magic: Magic;
    header: Header;
    imageSegments: ImageSegment[];
    textSegments: TextSegment[];
    endMarker: EndMarker;
}

struct Magic {
    value: uint8[4] = [0x4E, 0x49, 0x54, 0x46];
}

struct Header {
    fileLength: uint32;
    version: uint8[2];
    complexityLevel: uint8;
    standardType: uint8;
    originStationID: uint8[10];
    fileDateTime: uint8[14];
    fileTitle: uint8[80];
    securityInfo: SecurityInfo;
    fileCopyNumber: uint8;
    fileNumberOfCopies: uint8;
    encryption: uint8;
    backgroundColor: uint8[3];
    originatorName: uint8[24];
    originatorPhone: uint8[18];
    fileLengthExtended: uint32;
    headerLength: uint16;
    imageSegmentsCount: uint16;
    textSegmentsCount: uint16;
    reserved: uint8[6];
    userDefinedHeader: uint8[length: headerLength - 292];
}

struct SecurityInfo {
    classification: uint8;
    codewords: uint8[40];
    controlAndHandling: uint8[40];
    releaseInstructions: uint8[40];
    declassificationType: uint8;
    declassificationDate: uint8[8];
    declassificationExemption: uint8[4];
    downgrade: uint8;
    downgradeDate: uint8[8];
    classificationText: uint8[43];
    classificationAuthorityType: uint8;
    classificationAuthority: uint8[40];
    classificationReason: uint8[1];
    securitySourceDate: uint8[8];
    securityControlNumber: uint8[15];
}

struct ImageSegment {
    subheader: ImageSubheader;
    data: uint8[length: subheader.dataLength];
}

struct ImageSubheader {
    subheaderLength: uint16;
    imageID: uint8[10];
    imageDateTime: uint8[14];
    targetID: uint8[17];
    title: uint8[80];
    securityInfo: SecurityInfo;
    encryption: uint8;
    imageSource: uint8;
    numRows: uint32;
    numCols: uint32;
    pixelValueType: uint8;
    imageCompression: uint8;
    compressionRate: uint8[4];
    imageColor: uint8;
    numBands: uint8;
    bandInfo: BandInfo[];
    imageSyncCode: uint8;
    imageMode: uint8;
    numBlocksPerRow: uint8;
    numBlocksPerCol: uint8;
    numPixelsPerBlockHoriz: uint8;
    numPixelsPerBlockVert: uint8;
    numBitsPerPixel: uint8;
    displayLevel: uint8;
    attachmentLevel: uint8;
    imageLocation: uint8[10];
    imageMagnification: uint8[4];
    userDefinedHeader: uint8[length: subheaderLength - 256];
    dataLength: uint32;
}

struct BandInfo {
    bandRepresentation: uint8;
    bandSubcategory: uint8;
    bandFilterCondition: uint8;
    bandStandardImageColorCode: uint8;
    bandCompression: uint8;
    bandCompressionRate: uint8[4];
    bandData: uint8[length: numRows * numCols * (numBitsPerPixel / 8)];
}

struct TextSegment {
    subheader: TextSubheader;
    data: uint8[length: subheader.dataLength];
}

struct TextSubheader {
    subheaderLength: uint16;
    textID: uint8[10];
    textDateTime: uint8[14];
    title: uint8[80];
    securityInfo: SecurityInfo;
    encryption: uint8;
    textFormat: uint8;
    textExtendedSubheader: uint8[length: subheaderLength - 128];
    dataLength: uint32;
}

struct EndMarker {
    value: uint8[4] = [0x4E, 0x49, 0x54, 0x46];
}