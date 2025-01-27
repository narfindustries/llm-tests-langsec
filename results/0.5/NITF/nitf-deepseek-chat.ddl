struct NITFHeader {
    magic: uint8[4] == [0x4E, 0x49, 0x54, 0x46];
    version: uint8[2];
    complexityLevel: uint8;
    systemIdentifier: uint8[10];
    encryptionFlag: uint8;
    fileHeaderLength: uint16;
    fileTitle: uint8[80];
    classification: uint8[1];
    encryptionKey: uint8[24];
    fileCopyNumber: uint8[8];
    fileNumberOfCopies: uint8[8];
    originatorName: uint8[24];
    originatorPhone: uint8[18];
    fileLength: uint32;
    headerLength: uint16;
    numberOfImages: uint16;
    reserved: uint8[6];
}

struct ImageSubheader {
    imageIdentifier: uint8[10];
    imageDate: uint8[8];
    imageTitle: uint8[80];
    imageSecurity: uint8[1];
    imageEncryption: uint8[1];
    imageSource: uint8[42];
    imageCompression: uint8[1];
    imageBandCount: uint8[1];
    imagePixelDepth: uint8[1];
    imageRowCount: uint16;
    imageColumnCount: uint16;
    imageColorSpace: uint8[1];
    imageCompressionRate: uint8[4];
    reserved: uint8[12];
}

struct NITF {
    header: NITFHeader;
    imageSubheaders: ImageSubheader[header.numberOfImages];
    imageData: uint8[header.fileLength - header.headerLength - (header.numberOfImages * 204)];
}