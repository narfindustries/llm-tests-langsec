type NITF = {
    FileHeader: FileHeader;
    ImageSegment: ImageSegment[NUMI];
    SymbolSegment: SymbolSegment[NUMS];
    LabelSegment: LabelSegment[NUML];
    TextSegment: TextSegment[NUMT];
    DataExtensionSegment: DataExtensionSegment[NUMX];
    ReservedExtensionSegment: ReservedExtensionSegment[NUMDES];
};

type FileHeader = {
    FHDR: string(4) = "NITF";
    CLEVEL: string(2);
    STYPE: string(4);
    OSTAID: string(10);
    FDT: string(14);
    FTITLE: string(80);
    FSCLAS: string(1);
    FSCODE: string(40);
    FSCTLH: string(40);
    FSREL: string(40);
    FSRELDT: string(8);
    FSCAUT: string(20);
    FSCTLN: string(20);
    FSCOP: string(5);
    FSCPYS: string(5);
    ENCRYP: string(1);
    FBKGC: string(3);
    ONAME: string(24);
    OPHONE: string(18);
    FL: string(12);
    HL: string(6);
    NUMI: string(3);
    NUMS: string(3);
    NUML: string(3);
    NUMT: string(3);
    NUMX: string(3);
    NUMDES: string(3);
    UDHDL: string(5);
    UDHD: bytes(UDHDL);
    XHDL: string(5);
    XHD: bytes(XHDL);
};

type ImageSegment = {
    IID: string(10);
    INAME: string(80);
    ISCLAS: string(1);
    ISCODE: string(40);
    ISCTLH: string(40);
    ISREL: string(40);
    ISRELDT: string(8);
    ISCAUT: string(20);
    ISCTLN: string(20);
    ISCOP: string(5);
    ISCPS: string(5);
    ENCRYP: string(1);
    IREP: string(8);
    ICAT: string(8);
    IABPP: string(2);
    IPVTYPE: string(8);
    IREPBAND: string(3);
    ISYNC: string(4);
    IMODE: string(1);
    NBPR: string(4);
    NBPC: string(4);
    NPPBH: string(4);
    NPPBV: string(4);
    NBPP: string(2);
    IDLVL: string(3);
    IALVL: string(3);
    ILOC: string(10);
    IMAG: bytes((NBPR * NBPC * NPPBH * NPPBV * NBPP) / 8);
};

type SymbolSegment = {
    SID: string(10);
    SNAME: string(80);
    SSCLAS: string(1);
    SSCODE: string(40);
    SSCTLH: string(40);
    SSREL: string(40);
    SSRELDT: string(8);
    SSCAUT: string(20);
    SSCTLN: string(20);
    SSCOP: string(5);
    SSCPS: string(5);
    ENCRYP: string(1);
    SDATA: bytes(remaining);
};

type LabelSegment = {
    LID: string(10);
    LNAME: string(80);
    LSCLAS: string(1);
    LSCODE: string(40);
    LSCTLH: string(40);
    LSREL: string(40);
    LSRELDT: string(8);
    LSCAUT: string(20);
    LSCTLN: string(20);
    LSCOP: string(5);
    LSCPS: string(5);
    ENCRYP: string(1);
    LDATA: bytes(remaining);
};

type TextSegment = {
    TID: string(10);
    TNAME: string(80);
    TSCLAS: string(1);
    TSCODE: string(40);
    TSCTLH: string(40);
    TSREL: string(40);
    TSRELDT: string(8);
    TSCAUT: string(20);
    TSCTLN: string(20);
    TSCOP: string(5);
    TSCPS: string(5);
    ENCRYP: string(1);
    TDATA: bytes(remaining);
};

type DataExtensionSegment = {
    DEID: string(10);
    DESCLAS: string(1);
    DESCODE: string(40);
    DESCTLH: string(40);
    DESREL: string(40);
    DESRELDT: string(8);
    DESCAUT: string(20);
    DESCTLN: string(20);
    DESCOP: string(5);
    DESCPS: string(5);
    ENCRYP: string(1);
    DEDATA: bytes(remaining);
};

type ReservedExtensionSegment = {
    RESID: string(10);
    RESCLAS: string(1);
    RESCODE: string(40);
    RESCTLH: string(40);
    RESREL: string(40);
    RESRELDT: string(8);
    RESCAUT: string(20);
    RESCTLN: string(20);
    RESCOP: string(5);
    RESCPS: string(5);
    ENCRYP: string(1);
    RESDATA: bytes(remaining);
};