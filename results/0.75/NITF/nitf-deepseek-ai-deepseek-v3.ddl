type FileHeader {
    FHDR: string(4) = "NITF";
    FVER: string(5);
    CLEVEL: string(2);
    STYPE: string(4);
    OSTAID: string(10);
    FDATIM: string(14);
    FTITLE: string(80);
    FSCOP: string(5);
    FSCPYS: string(5);
    ENCRYP: string(1);
    FBKGC: string(3);
    ONAME: string(24);
    OPHONE: string(18);
    FL: uint64;
    HL: uint16;
    NUMI: uint16;
    NUMS: uint16;
    NUMX: uint16;
    NUMT: uint16;
    NUMDES: uint16;
    NUMRES: uint16;
    UDHDL: uint16;
    UDHD: bytes(UDHDL);
    XHDL: uint16;
    XHD: bytes(XHDL);
}

type ImageSubheader {
    IM: string(2);
    IID: string(10);
    IDATIM: string(14);
    TGTID: string(17);
    ILEVEL: uint8;
    ISORCE: string(42);
    NROWS: uint64;
    NCOLS: uint64;
    PVTYPE: string(3);
    IREP: string(8);
    ICAT: string(8);
    ABPP: uint8;
    PJUST: string(1);
    ICORDS: string(1);
    IGEOLO: string(60);
    NICOM: uint8;
    IC: bytes(NICOM * 1024);
    COMRAT: string(4);
    NBANDS: uint8;
    XBANDS: bytes(NBANDS * 6);
    IREPBAND: bytes(NBANDS);
    ISUBCAT: bytes(NBANDS * 6);
    IFC: bytes(NBANDS * 2);
    IMFLT: string(1);
    NLUTS: uint8;
    LUTD: bytes(NLUTS * 1024);
}

type ImageSegment {
    ImageSubheader: ImageSubheader;
    ImageData: bytes;
}

type GraphicSubheader {
    SY: string(2);
    SID: string(10);
    SNAME: string(20);
    SLOC: string(20);
    SCOLOR: string(3);
    SDRAW: string(1);
    SXHDL: uint16;
    SXHD: bytes(SXHDL);
}

type GraphicSegment {
    GraphicSubheader: GraphicSubheader;
    GraphicData: bytes;
}

type TextSubheader {
    TE: string(2);
    TXTALVL: uint8;
    TXTDT: string(14);
    TXTITL: string(80);
    TXSCLAS: string(1);
    TXSCODE: string(40);
    TXSCTLH: string(40);
    TXSREL: string(20);
    TXSDCTP: string(2);
    TXSDCDT: string(8);
    TXSDCXM: string(4);
    TXSDG: string(1);
    TXSDGDT: string(8);
    TXSCLTX: string(80);
    TXSCATP: string(20);
    TXSOP: string(28);
    TXSPHONE: string(18);
    TXSHDL: uint16;
    TXSHD: bytes(TXSHDL);
}

type TextSegment {
    TextSubheader: TextSubheader;
    TextData: bytes;
}

type DataExtensionSubheader {
    DE: string(2);
    DESVER: uint8;
    DESOFLW: string(1);
    DESITEM: uint8;
    DESSHL: uint16;
    DESSH: bytes(DESSHL);
}

type DataExtensionSegment {
    DataExtensionSubheader: DataExtensionSubheader;
    DataExtensionData: bytes;
}

type ReservedExtensionSubheader {
    RE: string(2);
    RESVER: uint8;
    RESOFLW: string(1);
    RESITEM: uint8;
    RESSHL: uint16;
    RESSH: bytes(RESSHL);
}

type ReservedExtensionSegment {
    ReservedExtensionSubheader: ReservedExtensionSubheader;
    ReservedExtensionData: bytes;
}

type UserDefinedHeader {
    UDHDL: uint16;
    UDHD: bytes(UDHDL);
}

type ExtendedHeader {
    XHDL: uint16;
    XHD: bytes(XHDL);
}

type NITF {
    FileHeader: FileHeader;
    ImageSegments: ImageSegment[NUMI];
    GraphicSegments: GraphicSegment[NUMS];
    TextSegments: TextSegment[NUMT];
    DataExtensionSegments: DataExtensionSegment[NUMDES];
    ReservedExtensionSegments: ReservedExtensionSegment[NUMRES];
    UserDefinedHeader: UserDefinedHeader;
    ExtendedHeader: ExtendedHeader;
}