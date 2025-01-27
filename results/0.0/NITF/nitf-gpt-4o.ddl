module NITF;

struct NITFFile {
    header: NITFHeader;
    fileSegments: [FileSegment] (count = header.fileSegmentCount);
}

struct NITFHeader {
    fileType: string (size = 4);
    version: string (size = 5);
    complexityLevel: string (size = 2);
    systemType: string (size = 1);
    originStationID: string (size = 10);
    fileDateTime: string (size = 14);
    fileTitle: string (size = 80);
    fileSecurity: SecurityInfo;
    fileSegmentCount: uint16;
}

struct SecurityInfo {
    classification: string (size = 1);
    classificationSystem: string (size = 2);
    codewords: string (size = 11);
    controlAndHandling: string (size = 2);
    releaseInstructions: string (size = 20);
    declassificationType: string (size = 2);
    declassificationDate: string (size = 8);
    declassificationExemption: string (size = 4);
    downgrade: string (size = 1);
    downgradeDate: string (size = 8);
}

struct FileSegment {
    segmentType: string (size = 1);
    segmentLength: uint32;
    segmentData: bytes (size = segmentLength);
}