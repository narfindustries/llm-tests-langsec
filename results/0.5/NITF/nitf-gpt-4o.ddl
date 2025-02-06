NITF = struct {
    FileHeader: struct {
        FHDR: string[8];
        CLEVEL: string[2];
        STYPE: string[4];
        OSTAID: string[10];
        FDT: string[14];
        FTITLE: string[80];
        FSCLAS: string[1];
        FSCLSY: string[2];
        FSCODE: string[11];
        FSCTLH: string[2];
        FSREL: string[20];
        FSDCTP: string[2];
        FSDCDT: string[8];
        FSDCXM: string[4];
        FSDG: string[6];
        FSDGDT: string[8];
        FSCLTX: string[43];
        FSCATP: string[1];
        FSCAUT: string[40];
        FSCRSN: string[1];
        FSSRDT: string[8];
        FSCTLN: string[15];
        ImageSegments: array of ImageSegment;
        GraphicSegments: array of GraphicSegment;
        TextSegments: array of TextSegment;
        DataExtensionSegments: array of DataExtensionSegment;
        ReservedExtensionSegments: array of ReservedExtensionSegment;
    };

    ImageSegment: struct {
        IM: string[2];
        IID1: string[10];
        IDATIM: string[14];
        // Additional fields for image segments
    };

    GraphicSegment: struct {
        // Fields for graphic segments
    };

    TextSegment: struct {
        TXTID: string[10];
        // Additional fields for text segments
    };

    DataExtensionSegment: struct {
        // Fields for data extension segments
    };

    ReservedExtensionSegment: struct {
        // Fields for reserved extension segments
    };
};