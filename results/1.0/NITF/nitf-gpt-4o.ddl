NITF : Endian = Big {
    FileHeader fileHeader;
    ImageSegment[] imageSegments;
    GraphicLayer[] graphicLayers <opt>;
    TextSegment[] textSegments <opt>;
    DataExtensionSegment[] dataExtensionSegments <opt>;
}

struct FileHeader {
    char[4] FHDR;
    char[2] CLEVEL;
    char[4] STYPE;
    char[10] OSTAID;
    char[14] FDT;
    char[80] FTITLE;
    char FSCLAS;
    char[2] FSCLSY;
    char[11] FSCODE;
    char[2] FSCTLH;
    char[20] FSREL;
    char[2] FSDCTP;
    char[8] FSDCDT;
    char[4] FSDCXM;
    char FSDG;
    char[8] FSDGDT;
    char[43] FSCLTX;
    char FSCATP;
    char[40] FSCAUT;
    char FSCRSN;
    char[8] FSSRDT;
    char[15] FSCTLN;
    char[5] FSCOP;
    char[5] FSCPYS;
    char ENCRYP;
}

struct ImageSegment {
    char[2] IM;
    char[10] IID;
    char[14] IDATIM;
    char[17] TGTID;
    char[80] ITITLE;
    char ISCLAS;
    char[11] ISCODE;
    char[2] ISCTLH;
    char[20] ISREL;
    char[2] ISDCTP;
    char[8] ISDCDT;
    char[4] ISDCXM;
    char ISDG;
    char[8] ISDGDT;
    char[43] ISCLTX;
    char[40] ISCAUT;
    char ISCATP;
    char ISCRSN;
    char[8] ISSRDT;
    char[15] ISCTLN;
    char[42] ISORCE;
    uint32 NROWS;
    uint32 NCOLS;
    char[3] PVTYPE;
    char[8] IREP;
    char[8] ICAT;
    uint8 ABPP;
    char PJUST;
    char ICORDS;
    char[60] IGEOLO;
    uint16 NICOM;
}

struct GraphicLayer {
    char[5] LID;
    char[14] LIDATIM;
    char[80] LCOMMENTS; 
}

struct TextSegment {
    char[2] TE;
    char[80] TEXTID;
    char[14] TEXTDATIM;
    char[80] TEXTTITLE;
}

struct DataExtensionSegment {
    char[2] DE;
    char[80] DESTITLE;
    char[14] DESDATIM;
    uint32 DESCHECK;
}