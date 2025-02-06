type NITF_Header = struct {
    FHDR: [21]byte;
    FVER: [5]byte;
    CLEVEL: [2]byte;
    STYPE: [4]byte;
    OSTAID: [10]byte;
    FDT: [14]byte;
    FTITLE: [80]byte;
    FSCLAS: byte;
    FSCLSY: [2]byte;
    FSCLCA: [11]byte;
    FSCLCO: [2]byte;
    FSCLRE: [20]byte;
    FSCLDE: [6]byte;
    ENCRYP: byte;
    FBKGC: [3]byte;
    ONAME: [27]byte;
    OPHONE: [18]byte;
    FLNG: [12]byte;
    HL: [6]byte;
    NUMI: [3]byte;
    LISH: [stoi(NUMI)]uint16;
    LI: [stoi(NUMI)]ImageSubHeader;
    NUMS: [3]byte;
    LSSH: [stoi(NUMS)]uint16;
    LS: [stoi(NUMS)]GraphicSubHeader;
    NUMX: [3]byte;
    NUMT: [3]byte;
    LTSH: [stoi(NUMT)]uint16;
    LT: [stoi(NUMT)]TextSubHeader;
    NUMDES: [3]byte;
    LDSH: [stoi(NUMDES)]uint16;
    LD: [stoi(NUMDES)]DESSubHeader;
    NUMRES: [3]byte;
    LRESH: [stoi(NUMRES)]uint16;
    LR: [stoi(NUMRES)]RESSubHeader;
};

type ImageSubHeader = struct {
    IM: [2]byte;
    IID1: [10]byte;
    IDATIM: [14]byte;
    TGTID: [17]byte;
    IID2: [80]byte;
    ISCLAS: byte;
    ISCTLH: [40]byte;
    ISCODE: [40]byte;
    ISCTLN: byte;
    ENCRYPT: byte;
    ISORCE: [42]byte;
    NROWS: [8]byte;
    NCOLS: [8]byte;
    PVTYPE: [3]byte;
    IREP: [8]byte;
    ICAT: [8]byte;
    ABPP: [2]byte;
    PJUST: byte;
    ICORDS: [1]byte;
    IGEN: byte;
    IPLN: [4]byte;
};

type GraphicSubHeader = struct {
    SY: [2]byte;
};

type TextSubHeader = struct {
    TEXTID: [7]byte;
    TXTALVL: [3]byte;
    TXTDT: [14]byte;
    TXTITL: [80]byte;
    TSCLAS: byte;
    TXTFG: [2]byte;
    TXTBG: [3]byte;
    ENCRYPT: byte;
    TXTFMT: [3]byte;
    TXSHDL: [5]byte;
    TXSOFL: [3]byte;
    TXSHD: [stoi(TXSHDL)]byte;
};

type DESSubHeader = struct {
    DESID: [25]byte;
    DESVER: [2]byte;
};

type RESSubHeader = struct {
    RE_TYPE: [25]byte;
    RE_VERSION: [2]byte;
};