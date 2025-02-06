type NITF {
    FileHeader: FileHeader;
    ImageSegments: ImageSegment[];
    GraphicSegments: GraphicSegment[];
    TextSegments: TextSegment[];
    DataExtensionSegments: DataExtensionSegment[];
    ReservedExtensionSegments: ReservedExtensionSegment[];
};

type FileHeader {
    FHDR: string(9) = "NITF02.10";
    FVER: string(4) = "02.10";
    CLEVEL: string(2) = "03";
    STYPE: string(4) = "BF01";
    OSTAID: string(10);
    FDT: string(14);
    FTITLE: string(80);
    SecurityMetadata: SecurityMetadata;
};

type SecurityMetadata {
    CLAS: string(1) = "U";
    CODE: string(40);
    CTLH: string(40);
    REL: string(40);
};

type ImageSegment {
    IM: ImageSubheader;
    ImageData: bytes;
};

type ImageSubheader {
    IID1: string(10);
    IDATIM: string(14);
    TGTID: string(17);
    SecurityMetadata: SecurityMetadata;
};

type GraphicSegment {
    GraphicSubheader: GraphicSubheader;
    GraphicData: bytes;
};

type GraphicSubheader {
    SID: string(10);
    SecurityMetadata: SecurityMetadata;
};

type TextSegment {
    TextSubheader: TextSubheader;
    TextData: bytes;
};

type TextSubheader {
    TXTID: string(10);
    SecurityMetadata: SecurityMetadata;
};

type DataExtensionSegment {
    DES: DataExtensionSubheader;
    DataExtensionData: bytes;
};

type DataExtensionSubheader {
    DESID: string(25);
    SecurityMetadata: SecurityMetadata;
};

type ReservedExtensionSegment {
    RES: ReservedExtensionSubheader;
    ReservedExtensionData: bytes;
};

type ReservedExtensionSubheader {
    RESID: string(25);
    SecurityMetadata: SecurityMetadata;
};