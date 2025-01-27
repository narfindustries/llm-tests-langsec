module NITF;

type NITFFile = struct {
    header: NITFHeader;
    fileParts: list of NITFFilePart(header.numParts);
};

type NITFHeader = struct {
    fileType: string(4); // "NITF"
    version: string(5);
    complexityLevel: string(2);
    systemType: string(4);
    originatingStationId: string(10);
    dateTime: string(14);
    fileTitle: string(80);
    fileSecurity: NITFSecurity;
    numParts: int(3);
};

type NITFSecurity = struct {
    classification: string(1);
    classificationSystem: string(2);
    codewords: string(11);
    controlAndHandling: string(2);
    releaseInstructions: string(20);
    declassificationType: string(2);
    declassificationDate: string(8);
    declassificationExemption: string(4);
    downgrade: string(1);
    downgradeDate: string(8);
    classificationText: string(43);
    classificationAuthType: string(1);
    classificationAuthority: string(40);
    classificationAuthorityNumber: string(20);
    classificationReason: string(1);
    securitySourceDate: string(8);
    securityControlNumber: string(15);
};

type NITFFilePart = union {
    when filePartType == "IM": NITFImage;
    when filePartType == "GR": NITFGraphic;
    when filePartType == "TX": NITFText;
    when filePartType == "DE": NITFDataExtension;
    when filePartType == "RE": NITFReservedExtension;
};

type NITFImage = struct {
    imageId: string(10);
    imageSecurity: NITFSecurity;
    imageRepresentation: string(2);
    imageCategory: string(2);
    actualBitsPerPixel: int(2);
    imageCoordinates: string(10);
    numImageRows: int(8);
    numImageColumns: int(8);
    imageCompression: string(2);
};

type NITFGraphic = struct {
    graphicId: string(10);
    graphicType: string(2);
    graphicSecurity: NITFSecurity;
    graphicData: bytes;
};

type NITFText = struct {
    textId: string(10);
    textSecurity: NITFSecurity;
    textFormat: string(3);
    textData: string;
};

type NITFDataExtension = struct {
    extensionId: string(25);
    extensionSecurity: NITFSecurity;
    extensionData: bytes;
};

type NITFReservedExtension = struct {
    reservedId: string(25);
    reservedSecurity: NITFSecurity;
    reservedData: bytes;
};

let filePartType = "IM"; // Placeholder, this should be determined by actual file data