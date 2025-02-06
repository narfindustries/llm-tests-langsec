FileHeader = struct {
    FHDR: string(9); // File Header Version
    CLEVEL: string(2); // Complexity Level
    STYPE: string(4); // Standard Type
    OSTAID: string(10); // Originating Station ID
    FDT: string(14); // File Date and Time
    FTITLE: string(80); // File Title
    FSCLAS: string(1); // File Security Classification
    FSCODE: string(40); // File Security Classification Code
    FSCTLH: string(40); // File Security Control and Handling
    FSREL: string(40); // File Releasing Instructions
    FSCAUT: string(20); // File Classification Authority
    FSCTLN: string(20); // File Security Control Number
    FSDWNG: string(6); // File Downgrade
    FSDWND: string(8); // File Downgrade Date
    FSDEVT: string(40); // File Downgrade Event
    FSCOP: string(5); // Copy Number
    FSCPYS: string(5); // Number of Copies
    ENCRYP: string(1); // Encryption
    FBKGDT: string(3); // Background Color
    ONAME: string(24); // Originator's Name
    OPHONE: string(18); // Originator's Phone
}

ImageSegmentSubheader = struct {
    IM: string(2); // Image Identifier
    IID1: string(10); // Image ID
    IDATIM: string(14); // Image Date and Time
    TGTID: string(17); // Target Identifier
    IID2: string(80); // Image ID 2
    ISCLAS: string(1); // Image Security Classification
    ISCODE: string(40); // Image Security Code
    // Additional fields for image details, compression, etc.
}

GraphicSegmentSubheader = struct {
    LISH: uint16; // Length of Image Subheader
    LI: uint32; // Length of Image Segment
    // Additional fields specific to graphics
}

TextSegmentSubheader = struct {
    LT: uint32; // Length of Text Segment
    LTSH: uint16; // Length of Text Subheader
    // Additional fields for text specifics
}

DataExtensionSegmentSubheader = struct {
    // Define fields specific to data extensions
}

ReservedExtensionSegmentSubheader = struct {
    // Define fields specific to reserved extensions
}

NITF = struct {
    header: FileHeader;
    imageSegments: array of ImageSegmentSubheader;
    graphicSegments: array of GraphicSegmentSubheader;
    textSegments: array of TextSegmentSubheader;
    dataExtensions: array of DataExtensionSegmentSubheader;
    reservedExtensions: array of ReservedExtensionSegmentSubheader;
}